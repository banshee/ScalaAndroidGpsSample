package com.restphone.sga

import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable.SynchronizedSet
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable

trait CallbackToken {
  def dispose
}

class CallbackManager[O, T] {
  type Callable = T => Unit
  type CallbackSet = mutable.Set[Callable] with SynchronizedSet[Callable]

  implicit def fnToCallable(x: T => Unit): Callable = x
  implicit def partialFnToCallable[U](x: PartialFunction[T, U]): Callable = y => if (x.isDefinedAt(y)) x(y)

  def add(owner: O, x: Callable): CallbackToken = {
    val callbackSet = callbacks.getOrElseUpdate(owner, defaultCallbackSet)
    callbackSet.add(x)
    new CallbackToken {
      def dispose = callbackSet.remove(x)
    }
  }

  def execute(value: T) = elements foreach { _(value) }
  def execute: Unit = execute(null.asInstanceOf[T])

  private[this] def defaultCallbackSet = new mutable.HashSet[Callable] with SynchronizedSet[Callable]

  private[this] val callbacks = new WeakHashMap[O, CallbackSet]() with SynchronizedMap[O, CallbackSet]

  private[this] def elements = for {
    (_, functions) <- callbacks
    fn <- functions
  } yield fn
}
