package com.restphone.sga

import android.location._
import scala.collection.JavaConversions._
import scala.collection.mutable.SynchronizedQueue
import android.os.Looper
import android.os.Bundle
import scala.PartialFunction._

sealed abstract class LocationResult
case class LocationMessage(location: Location) extends LocationResult
case class LocationEnabled(provider: String) extends LocationResult
case class LocationDisabled(provider: String) extends LocationResult
case class OutOfService(provider: String) extends LocationResult
case class Available(provider: String) extends LocationResult
case class TemporarilyUnavailable(provider: String) extends LocationResult
case class Power extends LocationResult

trait TurnLocationIntentsIntoLocationMessages {

}

class SynchronizedQueueWithChangeNotification[T](notify: SynchronizedQueueWithChangeNotification[T] => Unit) extends SynchronizedQueue[T] {
  val queue = new SynchronizedQueue[T]
  
  override def enqueue(p: T*) = {
    queue.enqueue(p:_*)
    notify(this)
  }

  override def dequeue = {
    val result = queue.dequeue
    notify(this)
    result
  }
} 

object RichLocation {
  val queue = new SynchronizedQueueWithChangeNotification[LocationResult](x => println("hello"))

  def getLastKnownLocations(manager: LocationManager): Iterable[LocationResult] =
    for {
      provider <- manager.getAllProviders
      location <- Option(manager.getLastKnownLocation(provider))
    } yield LocationMessage(location)

  trait EnqueueingLocationListener extends LocationListener {
    val q: SynchronizedQueueWithChangeNotification[LocationResult]

    abstract override def onLocationChanged(l: Location) = {
      q.enqueue(LocationMessage(l))
      super.onLocationChanged(l)
    }

    abstract override def onProviderDisabled(provider: String) = {
      q.enqueue(LocationDisabled(provider))
      super.onProviderDisabled(provider)
    }

    abstract override def onProviderEnabled(provider: String) = {
      q.enqueue(LocationEnabled(provider))
      super.onProviderEnabled(provider)
    }

    abstract override def onStatusChanged(provider: String, status: Int, extras: Bundle) = {
      condOpt(status) {
        case LocationProvider.OUT_OF_SERVICE => OutOfService(provider)
        case LocationProvider.AVAILABLE => Available(provider)
        case LocationProvider.TEMPORARILY_UNAVAILABLE => TemporarilyUnavailable(provider)
      } foreach {q.enqueue(_)}
      
      super.onStatusChanged(provider, status, extras)
    }
  }

  trait NotifyingLocationListener extends LocationListener {
    val notifyFn : () => Unit
    
    abstract override def onLocationChanged(l: Location) = {
      notifyFn()
      super.onLocationChanged(l)
    }

    abstract override def onProviderDisabled(provider: String) = {
      notifyFn()
      super.onProviderDisabled(provider)
    }

    abstract override def onProviderEnabled(provider: String) = {
      notifyFn()
      super.onProviderEnabled(provider)
    }

    abstract override def onStatusChanged(provider: String, status: Int, extras: Bundle) = {
      notifyFn()
      super.onStatusChanged(provider, status, extras)
    }
  }

  class DefaultLocationListener extends LocationListener {
    override def onLocationChanged(l: Location) {}
    override def onProviderDisabled(provider: String) {}
    override def onProviderEnabled(provider: String) {}
    override def onStatusChanged(provider: String, status: Int, extras: Bundle) {}
  }

  def createListener(q: SynchronizedQueueWithChangeNotification[LocationResult]) = new DefaultLocationListener with NotifyingLocationListener with EnqueueingLocationListener {
    val q = queue
    val notifyFn = () => {println("got something")}
  }

  def enqueueSingleLocation(locationManager: LocationManager, criteria: Criteria, looper: Looper) {
    Option(locationManager.getBestProvider(criteria, true)) foreach {
      x => locationManager.requestLocationUpdates(x, 0, 0, createListener(queue), looper);
    }
  }
}

