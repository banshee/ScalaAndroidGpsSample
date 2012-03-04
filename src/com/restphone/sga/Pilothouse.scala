package com.restphone.sga

import java.util.concurrent.Callable
import scala.collection.JavaConversions.asScalaConcurrentMap
import scala.collection.mutable
import scala.collection.mutable.SynchronizedSet
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable.SynchronizedMap

sealed abstract class CallbackElement[T]
case class CallbackElementFunctionWithArgument[T](x: T => Unit) extends CallbackElement[T]
case class CallbackElementPartialFunction[T](x: PartialFunction[T, _]) extends CallbackElement[T]
case class CallbackElementFunction[T](x: () => Unit) extends CallbackElement[T]
case class CallbackElementRunnable[T](x: Runnable) extends CallbackElement[T]
case class CallbackElementCallable[T](x: Callable[T]) extends CallbackElement[T]

object CallbackManager {
  implicit def createWithArgument[T](x: T => Unit) = CallbackElementFunctionWithArgument(x)
  implicit def createPartialFunction[T](x: PartialFunction[T, _]) = CallbackElementPartialFunction(x)
  implicit def createFunction[T](x: () => Unit) = CallbackElementFunction(x)
  implicit def createRunnable[T](x: Runnable) = CallbackElementRunnable(x)
  implicit def createCallable[T](x: Callable[T]) = CallbackElementCallable(x)
}

class CallbackManager[O, T] {
  type CallbackSet = mutable.Set[CallbackElement[T]] with SynchronizedSet[CallbackElement[T]]

  val callbacks = new WeakHashMap[O, CallbackSet]() with SynchronizedMap[O, CallbackSet] {
    override def default(k: O) = new mutable.HashSet[CallbackElement[T]] with SynchronizedSet[CallbackElement[T]]
  }
//    val cacheLoader = new CacheLoader[O, CallbackSet]() {
//      override def load(key: O): CallbackSet = new mutable.HashSet[CallbackElement[T]] with SynchronizedSet[CallbackElement[T]]
//    }
//
//    CacheBuilder.newBuilder.weakKeys.maximumSize(Integer.MAX_VALUE).

  def add(owner: O, x: CallbackElement[T]) = {
    callbacks.get(owner).get.add(x)
    x
  }

  // For Java compatibility
  def add(owner: O, x: Runnable): CallbackElement[T] = add(owner, x)
  def add(owner: O, x: Callable[T]): CallbackElement[T] = add(owner, x)

  def remove(owner: O, x: CallbackElement[T]) = callbacks.get(owner).get.remove(x)
  def remove(owner: O) = callbacks.remove(owner)

  def elements = for {
    (_, functions) <- callbacks
    fn <- functions
  } yield fn

  def execute(value: T) =
    elements foreach {
      case CallbackElementFunctionWithArgument(x) => x(value)
      case CallbackElementPartialFunction(pf) => { if (pf.isDefinedAt(value)) pf(value) else () }
      case CallbackElementFunction(x) => x()
      case CallbackElementRunnable(x) => x.run
      case CallbackElementCallable(x) => x.call
    }

  def execute: Unit = execute(null.asInstanceOf[T])
}
