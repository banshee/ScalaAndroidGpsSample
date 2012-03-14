package com.restphone.sga

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicLong
import scala.Option.option2Iterable
import scala.PartialFunction._
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable
import Frequency.ActiveUpdates
import Frequency.ExistingUpdates
import Frequency.NoUpdates
import Frequency.PassiveUpdates
import Frequency.UpdateFrequencySpec
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

/**
 * Location mixin traits for Android services, applications and fragments.
 *
 * Summary: mix in these traits to your service, application, activity or fragment, and
 * your onLocationEvent method will get called when your location changes.
 *
 * When you mix in the appropriate trait to your service, application or fragment,
 * your onLocationEvent is called with a LocationEvent parameter.  The defaults attach to
 * Android location services at the appropriate places in their lifecycles.  For activities and
 * fragments, that happens in the overridden onResume and onPause calls.
 *
 * Multiple objects can mix in these traits.  You can have a service and two fragments all
 * request location events, and the system will set the update frequencies equal to the
 * most aggressive.  That enables doing things like having a service ask for passive updates,
 * one fragment ask for updates every second, and another ask for once a minute.  As long as
 * the fragments are active, everyone will see the one-second updates.  When another activity
 * comes to the foreground, the requests for active updates will be stopped but the
 * service will continue to receive passive updates.
 */
object LocationListeners {
  import ListenerLifecycle._

  // Listers are attached to Android location managers.  When location managers call
  // the lister callbacks, the right LocationEvent is created and sent to registered
  // callbacks.

  /**
   * Events for valid locations and location statuses, including availabilty and service.
   */
  sealed abstract class LocationEvent
  case class LocationMessage(location: Location) extends LocationEvent
  case class LocationEnabled(provider: String) extends LocationEvent
  case class LocationDisabled(provider: String) extends LocationEvent
  case class OutOfService(provider: String) extends LocationEvent
  case class Available(provider: String) extends LocationEvent
  case class TemporarilyUnavailable(provider: String) extends LocationEvent

  // Objects interested in receiving LocationEvent messages register callbacks using 
  // the addLocationCallback methods and remove them using the removeLocationCallbacks
  // methods.
  def addLocationCallback(owner: AnyRef, fn: callbacks.Callable) = {
    callbacks.add(owner, fn)
  }

  private val callbacks = new CallbackManager[AnyRef, LocationEvent]

  /**
   * Removes the curret set of listeners, then uses the active requests to calculate which
   * listeners should be attached to location providers.
   *
   * While we could keep track of the existing providers and only apply changes, it doesn't
   * seem worthwhile.
   * @param locationManager
   * @param context
   * @param minTime
   */
  def resetRequestedUpdates(locationManager: LocationManager, context: Context) = {
    import Frequency._

    stopUpdateAndRemoveListenersFromQueue

    for {
      frequency <- shortestActiveFrequencyRequest
      provider <- locationManager.getAllProviders
    } {
      addListener(locationManager, provider, context, frequency)
    }
    shortestPassiveFrequencyRequest foreach {
      i =>
        addListener(locationManager, LocationManager.PASSIVE_PROVIDER, context, i)
    }

    activeFrequencyRequests collect {
      case ExistingUpdates =>
        def getLastKnownLocations(manager: LocationManager): Iterable[LocationEvent] =
          for {
            provider <- manager.getAllProviders
            location <- Option(manager.getLastKnownLocation(provider))
          } yield LocationMessage(location)
        getLastKnownLocations(locationManager) foreach callbacks.execute
    }
  }

  object ListenerLifecycle {
    def addListener(locationManager: LocationManager, provider: String, context: Context, minTime: Long) = {
      val listener = createNotifyingListener(locationManager)
      requestLocationUpdatesAndAddToActiveListeners(locationManager, provider, listener, context, minTime)
    }

    /**
     * A limited listener attaches to the best provider and only executes once.
     * @param locationManager
     * @param criteria
     * @param context
     */
    def addLimitedListener(locationManager: LocationManager, criteria: Criteria, context: Context) {
      Option(locationManager.getBestProvider(criteria, true)) foreach {
        provider =>
          val listener = createLimitedListener(locationManager, 1)
          requestLocationUpdatesAndAddToActiveListeners(locationManager, provider, listener, context, 0)
      }
    }

    def stopUpdateAndRemoveListenersFromQueue = {
      def stopUpdate(listener: NotifyingLocationListener) = { listener.locationManager.removeUpdates(listener); listener }
      activeListeners --= activeListeners map stopUpdate
    }

    def stopDeadListenersAndRemoveFromQueue = {
      val stoppedListeners = activeListeners collect canAndShouldBeStopped map { _.stop }
      activeListeners --= stoppedListeners
    }

    private[this] val activeListeners = mutable.Buffer.empty[NotifyingLocationListener]

    private[this] def requestLocationUpdatesAndAddToActiveListeners(locationManager: LocationManager, provider: String, listener: NotifyingLocationListener, context: Context, minTime: Long) {
      activeListeners += listener
      locationManager.requestLocationUpdates(provider, minTime, 0, listener, context.getMainLooper)
    }

    private[this] def createNotifyingListener(lm: LocationManager): NotifyingLocationListener =
      new BaseLocationListener with NotifyingLocationListener {
        override val locationManager = lm
      }

    /**
     * Creates a LocationListener that disconnects itself after receiving {@code nExecutions} {@code LocationMessage}s.
     * @param locationManager
     * @param nExecutions
     * @return
     */
    private[this] def createLimitedListener(locationManager: LocationManager, nExecutions: Long): NotifyingLocationListener = {
      val count = new AtomicLong(0)
      val parentLocationManager = locationManager
      new BaseLocationListener with CanBeStopped with NotifyingLocationListener {
        val locationManager = parentLocationManager
        def shouldBeStopped = (count.get >= nExecutions)
        override def notifyFn(l: LocationEvent) = {
          (l, count.get) match {
            case (LocationMessage(_), n) if n < nExecutions =>
              count.incrementAndGet
              super.notifyFn(l)
            case _ =>
          }
        }
      }
    }
  }

  def canAndShouldBeStopped: PartialFunction[NotifyingLocationListener, CanBeStopped] = {
    case x: CanBeStopped if x.shouldBeStopped => x
  }

  trait CanBeStopped extends NotifyingLocationListener {
    def stop = { locationManager.removeUpdates(this); this }
    def shouldBeStopped: Boolean
  }

  /**
   * Calls notifyFn on calls to the methods of LocationListener
   */
  trait NotifyingLocationListener extends LocationListener {
    def notifyFn(l: LocationEvent) = callbacks.execute(l)
    def locationManager: LocationManager

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
  case class PassiveUpdates(interval: Int) extends UpdateFrequencySpec
  case object ExistingUpdates extends UpdateFrequencySpec
  case object NoUpdates extends UpdateFrequencySpec

  def requestUpdateFrequency(owner: AnyRef, frequency: Set[UpdateFrequencySpec]) = {
    frequencyRequests.put(owner, frequency)
  }

  def shortestActiveFrequencyRequest: Option[Int] = shortestRequest(activeFrequencyRequestValues)
  def shortestPassiveFrequencyRequest: Option[Int] = shortestRequest(passiveFrequencyRequestValues)

  def activeFrequencyRequests = {
    val emptyStartingValue = Set.empty[UpdateFrequencySpec]
    frequencyRequests.values.foldLeft(emptyStartingValue)((memo, obj) => memo ++ obj)
  }

  private val activeFrequencyRequestValues: PartialFunction[UpdateFrequencySpec, Int] = {
    case ActiveUpdates(i) => i
  }

  private val passiveFrequencyRequestValues: PartialFunction[UpdateFrequencySpec, Int] = {
    case PassiveUpdates(i) => i
  }

  private def shortestRequest(converter: PartialFunction[UpdateFrequencySpec, Int]): Option[Int] = {
    def optionalMin(xs: Iterable[Int]): Option[Int] = {
      if (xs.isEmpty) None
      else Some(xs.min)
    }
    val intervals = Frequency.activeFrequencyRequests collect converter
    optionalMin(intervals)
  }

  private val frequencyRequests = new mutable.WeakHashMap[AnyRef, Set[UpdateFrequencySpec]]() with SynchronizedMap[AnyRef, Set[UpdateFrequencySpec]]

}

object LocationListenerMixins {
  import LocationListeners._

  trait StartLocationFeed {
    def locationFeedLocationManager: LocationManager
    def locationFeedApplicationContext: Context

    def locationUpdateFrequency: Set[UpdateFrequencySpec] = Set(PassiveUpdates(0))
    def onLocationEvent(locationEvent: LocationEvent) = {}
    val locationFeedDefaultCallbacks = List(onLocationEvent(_))
    var callbackDisposals: Iterable[CallbackToken] = List.empty
    def baseOnResume = {
      callbackDisposals = locationFeedDefaultCallbacks map { addLocationCallback(this, _) }
      Frequency.requestUpdateFrequency(this, locationUpdateFrequency)
      resetRequestedUpdates(locationFeedLocationManager, locationFeedApplicationContext)
    }
    def baseOnPause = {
      callbackDisposals foreach { _.dispose }
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
