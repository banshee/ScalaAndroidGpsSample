package com.restphone.sga

import scala.Option.option2Iterable
import scala.PartialFunction.condOpt
import scala.collection.JavaConversions._
import android.content.Context
import android.location.Criteria
import android.location.Location
import android.location.LocationListener
import android.location.LocationManager
import android.location.LocationProvider
import android.os.Bundle
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.SynchronizedSet
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.SynchronizedMap
import android.app.Activity
import android.support.v4.app.Fragment

object LocationPilothouse {
  import LocationPilothouseSupport._

  sealed abstract class LocationEvent
  case class LocationMessage(location: Location) extends LocationEvent
  case class LocationEnabled(provider: String) extends LocationEvent
  case class LocationDisabled(provider: String) extends LocationEvent
  case class OutOfService(provider: String) extends LocationEvent
  case class Available(provider: String) extends LocationEvent
  case class TemporarilyUnavailable(provider: String) extends LocationEvent
  case object Power extends LocationEvent

  sealed abstract class UpdateFrequencySpec
  case class ActiveUpdates(interval: Int) extends UpdateFrequencySpec
  case object PassiveUpdates extends UpdateFrequencySpec
  case object ExistingUpdates extends UpdateFrequencySpec
  case object NoUpdates extends UpdateFrequencySpec

  trait StartLocationFeed {
    def locationFeedActivity: Activity
    def locationFeedLocationManager: LocationManager = locationFeedActivity.getSystemService(Context.LOCATION_SERVICE).asInstanceOf[LocationManager]
    def locationFeedApplicationContext: Context = locationFeedActivity.getApplicationContext
    def baseOnResume = LocationPilothouse.startFetchingLocations(locationFeedLocationManager,
      locationFeedApplicationContext, 0);
    def baseOnPause = {}
  }

  trait StartLocationFeedForActivity extends Activity with StartLocationFeed {
    def locationFeedActivity = this
    abstract override def onResume = { baseOnResume; super.onResume }
    abstract override def onPause = { baseOnPause; super.onPause }
  }

  trait StartLocationFeedForFragment extends Fragment with StartLocationFeed {
    def locationFeedActivity = getActivity
    abstract override def onResume = { baseOnResume; super.onResume }
    abstract override def onPause = { baseOnPause; super.onPause }
  }

  private val frequencyRequests = new mutable.WeakHashMap[AnyRef, UpdateFrequencySpec]() with SynchronizedMap[AnyRef, UpdateFrequencySpec]

  def requestUpdateFrequency(owner: AnyRef, frequency: UpdateFrequencySpec) = {
    frequencyRequests.put(owner, frequency)
  }

  /**
   * Notice that there's a difference between listeners and callbacks.  Listeners are hooked up to
   * location manager updates, and they forward location events to callbacks.
   * Callbacks aren't affected by this method.
   * @param locationManager
   * @param context
   * @param minTime
   */
  def resetRequestedUpdates(locationManager: LocationManager, context: Context, minTime: Long) = {
    stopUpdateAndRemoveFromQueueAllListeners

    val shortestActiveFrequencyRequest = {
      val items = frequencyRequests.values.toSet
      val intervals = items.toList flatMap {
        case ActiveUpdates(i) => Some(i)
        case _ => None
      }
      intervals match {
        case h :: t => Some(intervals.min)
        case _ => None
      }
    }
    shortestActiveFrequencyRequest foreach { i => startFetchingLocations(locationManager, context, minTime) }

    frequencyRequests.values.toSet.foreach {
      (x: UpdateFrequencySpec) =>
        (x, shortestActiveFrequencyRequest) match {
          case (PassiveUpdates, Some(interval)) if interval <= 2000 =>
          // Do nothing, since we're asking for active updates fairly often

          case (PassiveUpdates, _) =>
            val listener = createNotifyingListener(locationManager)
            requestLocationUpdatesAndAddToQueue(locationManager, android.location.LocationManager.PASSIVE_PROVIDER, listener, context, minTime)

          case (ExistingUpdates, _) =>
            getLastKnownLocations(locationManager) foreach callbacks.execute

          case (ActiveUpdates(_), _) =>
          // Already handled by currentFrequencyRequest

          case (NoUpdates, _) =>
        }
    }
  }

  def getLastKnownLocations(manager: LocationManager): Iterable[LocationEvent] =
    for {
      provider <- manager.getAllProviders
      location <- Option(manager.getLastKnownLocation(provider))
    } yield LocationMessage(location)

  private trait NotifyUsingCallbacksField {
    def notifyFn(l: LocationEvent) = callbacks.execute(l)
  }

  private trait EatDuplicateNotifications extends HasNotifyWithLocation {
    private def locationsMatch(a: Location, b: Location) =
      a.getTime == b.getTime && a.getProvider == b.getProvider

    private var eatDuplicateNotificationsPreviousNotification: Option[LocationEvent] = None

    abstract override def notifyFn(l: LocationEvent) = {
      (eatDuplicateNotificationsPreviousNotification, l) match {
        case (Some(LocationMessage(previousLocation)), LocationMessage(newLocation)) if locationsMatch(previousLocation, newLocation) =>
        // Do nothing.  We've got a duplicate message
        case (_, LocationMessage(_)) =>
          eatDuplicateNotificationsPreviousNotification = Some(l)
          super.notifyFn(l)
        case _ =>
          super.notifyFn(l)
      }
    }
  }

  trait HasShouldBeRemovedMethod {
    def shouldBeRemoved: Boolean
  }

  private trait HasStopMethod extends LocationListenerWithLocationManager {
    this: LocationListener =>
    def stop = locationManager.removeUpdates(this)
  }

  private trait RemoveAfterNCalls extends NotifyUsingCallbacksField {
    this: LocationListener =>
    override def notifyFn(l: LocationEvent) = {
      super.notifyFn(l)
      currentFetchers dequeueAll {
        case x: HasShouldBeRemovedMethod with HasStopMethod if x.shouldBeRemoved =>
          x.stop
          true
        case _ => false
      }
    }
  }

  def stopUpdate(listener: LocationListenerWithLocationManager) = listener.locationManager.removeUpdates(listener)

  private val callbacks = new CallbackManager[AnyRef, LocationEvent]

  private def createNotifyingListener(lm: LocationManager): LocationListenerWithLocationManager =
    new BaseLocationListener with NotifyingLocationListener with NotifyUsingCallbacksField with RemoveAfterNCalls with EatDuplicateNotifications with LocationListenerWithLocationManager {
      override val locationManager = lm
    }

  /**
   * Creates a LocationListener that disconnects itself after receiving {@code nExecutions} {@code LocationMessage}s.
   * @param locationManager
   * @param nExecutions
   * @return
   */
  private def createRemovableListener(locationManager: LocationManager, nExecutions: Long): LocationListenerWithLocationManager = {
    var count = new AtomicLong(0)
    val locationManager2 = locationManager
    var result = new BaseLocationListener with NotifyingLocationListener with NotifyUsingCallbacksField with HasShouldBeRemovedMethod with LocationListenerWithLocationManager {
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
    result
  }

  def fetchSingleLocation(locationManager: LocationManager, criteria: Criteria, context: Context) {
    Option(locationManager.getBestProvider(criteria, true)) foreach {
      provider =>
        val listener = createRemovableListener(locationManager, 1)
        requestLocationUpdatesAndAddToQueue(locationManager, provider, listener, context, 0)
    }
  }

  private val currentFetchers = new mutable.SynchronizedQueue[LocationListenerWithLocationManager]

  def startFetchingLocations(locationManager: LocationManager, context: Context, minTime: Long, specificProvider: Option[String] = None) {
    val providers: Iterable[String] = specificProvider match {
      case Some(p) => Iterable(p)
      case None => locationManager.getAllProviders
    }
    providers foreach {
      provider =>
        val listener = createNotifyingListener(locationManager)
        requestLocationUpdatesAndAddToQueue(locationManager, provider, listener, context, minTime)
    }
  }

  def requestLocationUpdatesAndAddToQueue(locationManager: LocationManager, provider: String, listener: LocationListenerWithLocationManager, context: Context, minTime: Long) {
    currentFetchers += listener
    locationManager.requestLocationUpdates(provider, minTime, 0, listener, context.getMainLooper)
  }

  def stopUpdateAndRemoveFromQueue(listeners: List[LocationListenerWithLocationManager]) = {
    currentFetchers dequeueAll listeners.contains
    listeners foreach stopUpdate
  }
  def stopUpdateAndRemoveFromQueueAllListeners = stopUpdateAndRemoveFromQueue(currentFetchers.toList)

  def addListener(owner: AnyRef, fn: LocationEvent => Unit) = {
    callbacks.add(owner, CallbackElementFunctionWithArgument(fn))
  }
  def addListener(owner: AnyRef, fn: CallbackManager.CallbackWithArgument[LocationEvent]) = {
    callbacks.add(owner, CallbackElementWithCustomCallback(fn))
  }

  def removeListeners(owner: AnyRef) = callbacks.remove(owner)
  def removeListener(owner: AnyRef, c: CallbackElement[LocationEvent]) = callbacks.remove(owner, c)
}

private object LocationPilothouseSupport {
  import LocationPilothouse._

  trait LocationListenerWithLocationManager extends LocationListener {
    def locationManager: LocationManager
  }

  trait HasNotifyWithLocation {
    def notifyFn(l: LocationEvent)
  }

  /**
   * Calls notifyFn on calls to the methods of LocationListener
   */
  abstract trait NotifyingLocationListener extends LocationListener with HasNotifyWithLocation {
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

  val returnTrue = (x: Unit) => true

  val t = Some("ab") filter { s => s forall { _ != ' ' } }
}
