package com.restphone.sga

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicLong
import scala.Option.option2Iterable
import scala.PartialFunction.condOpt
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable
import Frequency.ActiveUpdates
import Frequency.ExistingUpdates
import Frequency.NoUpdates
import Frequency.PassiveUpdates
import Frequency.UpdateFrequencySpec
import Listeners.startFetchingLocations
import android.app.Activity
import android.content.Context
import android.location.LocationListener
import android.location.Criteria
import android.location.Location
import android.location.LocationManager
import android.location.LocationProvider
import android.os.Bundle
import android.support.v4.app.Fragment
import android.app.Service

object Listeners {
  sealed abstract class LocationEvent
  case class LocationMessage(location: Location) extends LocationEvent
  case class LocationEnabled(provider: String) extends LocationEvent
  case class LocationDisabled(provider: String) extends LocationEvent
  case class OutOfService(provider: String) extends LocationEvent
  case class Available(provider: String) extends LocationEvent
  case class TemporarilyUnavailable(provider: String) extends LocationEvent

  def addLocationCallback(owner: AnyRef, fn: LocationEvent => Unit) = {
    callbacks.add(owner, CallbackElementFunctionWithArgument(fn))
  }
  def addLocationCallback(owner: AnyRef, fn: CallbackManager.CallbackWithArgument[LocationEvent]) = {
    callbacks.add(owner, CallbackElementWithCustomCallback(fn))
  }
  def addLocationCallback(owner: AnyRef, fn: CallbackElement[LocationEvent]) = {
    callbacks.add(owner, fn)
  }

  def removeLocationCallbacks(owner: AnyRef) = callbacks.remove(owner)
  def removeLocationCallback(owner: AnyRef, c: CallbackElement[LocationEvent]) = callbacks.remove(owner, c)

  private val activeListeners = new mutable.SynchronizedQueue[LocationListenerWithLocationManager]
  private val callbacks = new CallbackManager[AnyRef, LocationEvent]

  /**
   * Notice that there's a difference between listeners and callbacks.  Listeners are hooked up to
   * location manager updates, and they forward location events to callbacks.
   * Callbacks aren't affected by this method.
   * @param locationManager
   * @param context
   * @param minTime
   */
  def resetRequestedUpdates(locationManager: LocationManager, context: Context) = {
    import Frequency._

    stopUpdateAndRemoveAllListenersFromQueue

    shortestActiveFrequencyRequest foreach { i => startFetchingLocations(locationManager, context, i) }

    activeFrequencyRequests foreach {
      (x: UpdateFrequencySpec) =>
        x match {
          case PassiveUpdates =>
            val listener = createNotifyingListener(locationManager)
            requestLocationUpdatesAndAddToActiveListeners(locationManager, android.location.LocationManager.PASSIVE_PROVIDER, listener, context, 1000)

          case ExistingUpdates =>
            def getLastKnownLocations(manager: LocationManager): Iterable[LocationEvent] =
              for {
                provider <- manager.getAllProviders
                location <- Option(manager.getLastKnownLocation(provider))
              } yield LocationMessage(location)
            getLastKnownLocations(locationManager) foreach callbacks.execute

          case ActiveUpdates(_) =>
          // Already handled by currentFrequencyRequest

          case NoUpdates =>
        }
    }
  }

  def startFetchingLocations(locationManager: LocationManager, context: Context, minTime: Long, specificProvider: Option[String] = None) {
    val providers: Iterable[String] = specificProvider match {
      case Some(p) => Iterable(p)
      case None => locationManager.getAllProviders
    }
    providers foreach {
      provider =>
        val listener = createNotifyingListener(locationManager)
        requestLocationUpdatesAndAddToActiveListeners(locationManager, provider, listener, context, minTime)
    }
  }

  def requestLocationUpdatesAndAddToActiveListeners(locationManager: LocationManager, provider: String, listener: LocationListenerWithLocationManager, context: Context, minTime: Long) {
    activeListeners += listener
    locationManager.requestLocationUpdates(provider, minTime, 0, listener, context.getMainLooper)
  }

  def stopUpdateAndRemoveAllListenersFromQueue = {
    def stopUpdateAndRemoveFromQueue(listeners: List[LocationListenerWithLocationManager]) = {
      def stopUpdate(listener: LocationListenerWithLocationManager) = listener.locationManager.removeUpdates(listener)
      activeListeners dequeueAll listeners.contains
      listeners foreach stopUpdate
    }
    stopUpdateAndRemoveFromQueue(activeListeners.toList)
  }

  def createNotifyingListener(lm: LocationManager): LocationListenerWithLocationManager =
    new BaseLocationListener with LocationListenerWithLocationManager with NotifyingLocationListener with NotifyUsingCallbacksField with RemoveAfterNEvents with EatDuplicateNotifications {
      override val locationManager = lm
    }

  /**
   * Creates a LocationListener that disconnects itself after receiving {@code nExecutions} {@code LocationMessage}s.
   * @param locationManager
   * @param nExecutions
   * @return
   */
  private def createRemovableListener(locationManager: LocationManager, nExecutions: Long): LocationListenerWithLocationManager = {
    val count = new AtomicLong(0)
    val locationManager2 = locationManager
    new BaseLocationListener with NotifyingLocationListener with NotifyUsingCallbacksField with CanBeStopped with LocationListenerWithLocationManager {
      val locationManager = locationManager2
      def shouldBeRemoved = {
        (count.get >= nExecutions)
      }
      override def notifyFn(l: LocationEvent) = {
        super.notifyFn(l)
        l match {
          case LocationMessage(_) => count.incrementAndGet
          case _ =>
        }
      }
    }
  }

  def fetchSingleLocation(locationManager: LocationManager, criteria: Criteria, context: Context) {
    Option(locationManager.getBestProvider(criteria, true)) foreach {
      provider =>
        val listener = createRemovableListener(locationManager, 1)
        requestLocationUpdatesAndAddToActiveListeners(locationManager, provider, listener, context, 0)
    }
  }

  /**
   * Returns true if the location given is equivalent to the previous location given.
   * Saves the location given and will use it for the next run.
   * Returns false on the first run.
   * Any event other than LocationMessage(_) returns false and is not saved.
   *
   * Equivalence is defined in the {@code locationsMatch} method.
   */
  private val locationIsARepeat = {
    def locationsMatch(a: Location, b: Location) =
      a.getTime == b.getTime && a.getAccuracy == b.getAccuracy

    val eatDuplicateNotificationsPreviousNotification: AtomicReference[LocationEvent] = new AtomicReference(null)

    (l: LocationEvent) => eatDuplicateNotificationsPreviousNotification.synchronized {
      (eatDuplicateNotificationsPreviousNotification.get, l) match {
        case (LocationMessage(previousLocation), LocationMessage(newLocation)) if locationsMatch(previousLocation, newLocation) =>
          true
        case (_, LocationMessage(_)) =>
          eatDuplicateNotificationsPreviousNotification.set(l)
          false
        case _ =>
          false
      }
    }
  }

  trait CanBeStopped extends LocationListenerWithLocationManager {
    this: LocationListener =>
    def stop = locationManager.removeUpdates(this)
    def shouldBeRemoved: Boolean
  }

  trait NotifyUsingCallbacksField extends HasNotifyWithLocationEventParameter {
    def notifyFn(l: LocationEvent) = callbacks.execute(l)
  }

  trait EatDuplicateNotifications extends HasNotifyWithLocationEventParameter {
    abstract override def notifyFn(l: LocationEvent) = {
      if (!locationIsARepeat(l))
        super.notifyFn(l)
    }
  }

  trait RemoveAfterNEvents extends HasNotifyWithLocationEventParameter {
    abstract override def notifyFn(l: LocationEvent) = {
      super.notifyFn(l)
      activeListeners dequeueAll {
        case x: CanBeStopped if x.shouldBeRemoved =>
          x.stop
          true
        case _ => false
      }
    }
  }

  trait LocationListenerWithLocationManager extends LocationListener {
    def locationManager: LocationManager
  }

  trait HasNotifyWithLocationEventParameter {
    def notifyFn(l: LocationEvent)
  }

  /**
   * Calls notifyFn on calls to the methods of LocationListener
   */
  trait NotifyingLocationListener extends LocationListener with HasNotifyWithLocationEventParameter {
    abstract override def onLocationChanged(l: Location) = {
      notifyFn(LocationMessage(l))
      super.onLocationChanged(l)
    }

    abstract override def onProviderDisabled(provider: String) = {
      notifyFn(LocationDisabled(provider))
      super.onProviderDisabled(provider)

    }

    abstract override def onProviderEnabled(provider: String) = {
      notifyFn(LocationEnabled(provider))
      super.onProviderEnabled(provider)
    }

    abstract override def onStatusChanged(provider: String, status: Int, extras: Bundle) = {
      condOpt(status) {
        case LocationProvider.OUT_OF_SERVICE => OutOfService(provider)
        case LocationProvider.AVAILABLE => Available(provider)
        case LocationProvider.TEMPORARILY_UNAVAILABLE => TemporarilyUnavailable(provider)
      } foreach { notifyFn(_) }

      super.onStatusChanged(provider, status, extras)
    }
  }

  class BaseLocationListener extends LocationListener {
    override def onLocationChanged(l: Location) {}
    override def onProviderDisabled(provider: String) {}
    override def onProviderEnabled(provider: String) {}
    override def onStatusChanged(provider: String, status: Int, extras: Bundle) {}
  }

}

object Frequency {
  sealed abstract class UpdateFrequencySpec
  case class ActiveUpdates(interval: Int) extends UpdateFrequencySpec
  case object PassiveUpdates extends UpdateFrequencySpec
  case object ExistingUpdates extends UpdateFrequencySpec
  case object NoUpdates extends UpdateFrequencySpec

  private val frequencyRequests = new mutable.WeakHashMap[AnyRef, Set[UpdateFrequencySpec]]() with SynchronizedMap[AnyRef, Set[UpdateFrequencySpec]]

  def activeFrequencyRequests = {
    val emptyStartingValue = Set.empty[UpdateFrequencySpec]
    frequencyRequests.values.foldLeft(emptyStartingValue)((memo, obj) => memo ++ obj)
  }

  def shortestActiveFrequencyRequest = {
    val intervals = Frequency.activeFrequencyRequests collect {
      case ActiveUpdates(i) => i
    }
    if (intervals.size > 0) Some(intervals.min)
    else None
  }

  def requestUpdateFrequency(owner: AnyRef, frequency: Set[UpdateFrequencySpec]) = {
    frequencyRequests.put(owner, frequency)
  }
}

object AndroidInteractions {
  import Listeners._

  trait StartLocationFeed {
    def locationFeedLocationManager: LocationManager
    def locationFeedApplicationContext: Context

    def locationUpdateFrequency: Set[UpdateFrequencySpec] = Set(PassiveUpdates)
    def onLocationEvent(locationEvent: LocationEvent) = {}
    val locationFeedDefaultCallbacks: Iterable[CallbackElement[LocationEvent]] = List(CallbackElementFunctionWithArgument(onLocationEvent))
    def baseOnResume = {
      locationFeedDefaultCallbacks foreach { addLocationCallback(this, _) }
      Frequency.requestUpdateFrequency(this, locationUpdateFrequency)
      resetRequestedUpdates(locationFeedLocationManager, locationFeedApplicationContext)
    }
    def baseOnPause = {
      locationFeedDefaultCallbacks foreach { removeLocationCallback(this, _) }
      Frequency.requestUpdateFrequency(this, Set(NoUpdates))
    }
  }

  trait StartLocationFeedForActivity extends Activity with StartLocationFeed {
    def locationFeedLocationManager = this.getSystemService(Context.LOCATION_SERVICE).asInstanceOf[LocationManager]
    def locationFeedApplicationContext: Context = this.getApplicationContext
    abstract override def onResume = { baseOnResume; super.onResume }
    abstract override def onPause = { baseOnPause; super.onPause }
  }

  trait StartLocationFeedForFragment extends Fragment with StartLocationFeed {
    def locationFeedLocationManager = this.getActivity.getSystemService(Context.LOCATION_SERVICE).asInstanceOf[LocationManager]
    def locationFeedApplicationContext: Context = this.getActivity.getApplicationContext
    abstract override def onResume = { baseOnResume; super.onResume }
    abstract override def onPause = { baseOnPause; super.onPause }
  }

  trait StartLocationFeedForService extends Service with StartLocationFeed {
    def locationFeedLocationManager = this.getSystemService(Context.LOCATION_SERVICE).asInstanceOf[LocationManager]
    def locationFeedApplicationContext: Context = this.getApplicationContext
    abstract override def onCreate = { baseOnResume; super.onCreate }
    abstract override def onDestroy = { baseOnPause; super.onDestroy }
  }

  trait LocationFeedConstantUpdates extends StartLocationFeed {
    override def locationUpdateFrequency = super.locationUpdateFrequency ++ Set(ActiveUpdates(5000))
  }

  trait LocationFeedExistingLocations extends StartLocationFeed {
    override def locationUpdateFrequency = super.locationUpdateFrequency ++ Set(ExistingUpdates)
  }
}
