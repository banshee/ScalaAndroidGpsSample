package com.restphone.sga

import com.restphone.sga.CallbackManager.CallbackWithArgument
import android.app.Activity
import android.content.Context
import android.location.LocationManager
import android.os.Bundle
import android.support.v4.app.FragmentActivity
import android.support.v4.app.Fragment

trait StartLocationFeed {
  def locationFeedActivity: Activity
  def locationFeedLocationManager: LocationManager = locationFeedActivity.getSystemService(Context.LOCATION_SERVICE).asInstanceOf[LocationManager]
  def locationFeedApplicationContext: Context = locationFeedActivity.getApplicationContext
  def baseOnResume = RichLocation.startFetchingLocations(locationFeedLocationManager,
    null,
    locationFeedApplicationContext);
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

class Fnx extends Fragment with StartLocationFeedForFragment {
}

class ScalaGpsAndroidActivity extends FragmentActivity with StartLocationFeedForActivity {
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    RichLocation.addListener(this, x => System.out.println("out3: " + x))
    setContentView(R.layout.main)
  }
}