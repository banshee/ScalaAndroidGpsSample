package com.restphone.sga

import com.restphone.sga.CallbackManager.CallbackWithArgument
import android.app.Activity
import android.content.Context
import android.location.LocationManager
import android.os.Bundle
import android.support.v4.app.FragmentActivity
import android.support.v4.app.Fragment
import com.restphone.sga.AndroidInteractions._
import com.restphone.sga.Listeners._
import android.widget.EditText
import android.view.LayoutInflater
import android.view.ViewGroup
import android.app.Service
import android.content.Intent
import android.os.IBinder

class ScalaGpsAndroidActivity extends FragmentActivity with StartLocationFeedForActivity with LocationFeedConstantUpdates with LocationFeedExistingLocations {
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)
  }

  override def onStart = {
    super.onStart
    val fragmentManager = getSupportFragmentManager
    val fragmentTransaction = fragmentManager.beginTransaction
    val f = new SimpleLocationDisplay
    fragmentTransaction.add(R.id.mainContainer, f)
    fragmentTransaction.commit
    
    val i = (new Intent).setClass(getApplicationContext, classOf[SimpleService])
    startService(i)
  }

  override def onLocationEvent(locationEvent: LocationEvent) = {
    println("Location reported from activity: " + locationEvent)
  }
}

class SimpleLocationDisplay extends Fragment with StartLocationFeedForFragment {
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle) = {
    // A standard fragment, nothing special
    inflater.inflate(R.layout.simple_location_report, container, false);
  }

  override def onLocationEvent(locationEvent: LocationEvent) = {
    for {
      activity <- Option(getActivity)
      view <- Option(activity.findViewById(R.id.timestamp))
      editText <- Option(view.asInstanceOf[EditText])
    } {
      locationEvent match {
        case LocationMessage(newLocation) => editText.setText(newLocation.toString)
        case _ =>
      }
    }

    // Printing to logcat so you can see events stop and start through the Android lifecycle
    println("Location reported from fragment: " + locationEvent)
  }
}

class SimpleService extends Service with StartLocationFeedForService {
  override def onBind(i: Intent) = null
  override def onStartCommand(i: Intent, flags: Int, startId: Int) = Service.START_STICKY
  override def onLocationEvent(locationEvent: LocationEvent) = println("Location reported from service: " + locationEvent)
}