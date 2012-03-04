package com.restphone.sga

import scala.Option.option2Iterable
import scala.PartialFunction.condOpt
import scala.collection.JavaConversions.asScalaBuffer
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

sealed abstract class LocationResult
case class LocationMessage(location: Location) extends LocationResult
case class LocationEnabled(provider: String) extends LocationResult
case class LocationDisabled(provider: String) extends LocationResult
case class OutOfService(provider: String) extends LocationResult
case class Available(provider: String) extends LocationResult
case class TemporarilyUnavailable(provider: String) extends LocationResult
case object Power extends LocationResult

object RichLocation {
  def getLastKnownLocations(manager: LocationManager): Iterable[LocationResult] =
    for {
      provider <- manager.getAllProviders
      location <- Option(manager.getLastKnownLocation(provider))
    } yield LocationMessage(location)

  abstract trait NotifyingLocationListener extends LocationListener {
    def notifyFn(l: LocationResult)

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

  private val callbacks = new CallbackManager[AnyRef, LocationResult]

  private trait NotifyUsingCallbacksField {
    def notifyFn(l: LocationResult) = callbacks.execute(l)
  }

  private def createNotifyingListener = new BaseLocationListener with NotifyingLocationListener with NotifyUsingCallbacksField

  private def createLimitedListener(locationManager: LocationManager, nExecutions: Int) = {
    var innerListener = new AtomicReference[LocationListener]
    var count = new AtomicInteger(0)
    var result = new BaseLocationListener with NotifyingLocationListener with NotifyUsingCallbacksField {
      override def notifyFn(l: LocationResult) = {
        super.notifyFn(l)
        l match {
          case LocationMessage(_) =>
            if (count.incrementAndGet >= nExecutions) locationManager.removeUpdates(innerListener.get)
          case _ =>
        }
      }
    }
    innerListener.set(result)
    result
  }

  def fetchSingleLocation(locationManager: LocationManager, criteria: Criteria, context: Context) {
    Option(locationManager.getBestProvider(criteria, true)) foreach {
      locationManager.requestLocationUpdates(_, 0, 0, createLimitedListener(locationManager, 1), context.getMainLooper());
    }
  }

  val currentFetchers = new mutable.SynchronizedQueue[LocationListener]

  def startFetchingLocations(locationManager: LocationManager, criteria: Criteria, context: Context) {
    locationManager.getAllProviders.foreach {
      val listener = createNotifyingListener
      provider => locationManager.requestLocationUpdates(provider, 0, 0, listener, context.getMainLooper)
      currentFetchers += listener
    }
  }

  def stopFetchingLocations(locationManager: LocationManager, context: Context) {
    currentFetchers dequeueAll {
      x =>
        locationManager.removeUpdates(x)
        true
    }
  }
  
  def addListener(owner: AnyRef, fn: LocationResult => Unit) = {
    callbacks.add(owner, CallbackElementFunctionWithArgument(fn))
  }
  def addListener(owner: AnyRef, fn: CallbackManager.CallbackWithArgument[LocationResult]) = {
    callbacks.add(owner, CallbackElementWithCustomCallback(fn))
  }
  def removeListeners(owner: AnyRef) = callbacks.remove(owner)
  def removeListener(owner: AnyRef, c: CallbackElement[LocationResult]) = callbacks.remove(owner, c)
}

