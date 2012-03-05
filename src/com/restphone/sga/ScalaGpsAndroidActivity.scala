package com.restphone.sga

import com.restphone.sga.CallbackManager.CallbackWithArgument
import com.restphone.sga.LocationPilothouse._

import android.app.Activity
import android.content.Context
import android.location.LocationManager
import android.os.Bundle
import android.support.v4.app.FragmentActivity
import android.support.v4.app.Fragment

class Fnx extends Fragment with StartLocationFeedForFragment {
}

class ScalaGpsAndroidActivity extends FragmentActivity with StartLocationFeedForActivity {
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    LocationPilothouse.addListener(this, x => System.out.println("out3: " + x))
    setContentView(R.layout.main)
  }
}