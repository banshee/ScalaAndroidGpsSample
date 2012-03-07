# Sample code for getting location events in Android using Scala

## Getting started

You'll want a Scala build environment.  I use [AndroidProguardScala].

[AndroidProguardScala]: https://github.com/banshee/AndroidProguardScala

## What this provides

Location.scala has the source code for traits that can be mixed in with fragments, activities and services.

If you add these mixins, you'll get a new method (with the default behavior of 'do nothing') 'onLocationEvent'.

Override onLocationEvent to do something useful as locations arrive.  That's it!  
Multiple fragments, activities and services can use these mixins simultaneously.

### Traits

* StartLocationFeedForService
* StartLocationFeedForFragment
* StartLocationFeedForActivity

Mix these into your activity, fragment or service.

### Adjusting frequency of locations

* LocationFeedConstantUpdates - updates every five seconds
* LocationFeedExistingLocations - only updates with locations that are currently available

The default behavior is to hook up to passive location events, so you'll only see locations if someone else is asking for them.  
That's probably only useful for services.

## What it does behind the scenes

The traits override the onPause/onResume calls to your fragment/activity, and onCreate/onDestroy for your services.
They attach listeners to the location providers, turn the data from the location providers into LocationEvent objects, and 
call your onLocationEvent method.

They automatically handle disconnecting from the Android location manager as they move through the Android lifecycle. 
