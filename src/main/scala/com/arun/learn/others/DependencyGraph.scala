package com.arun.learn.others

import com.arun.learn.ZIO
import com.arun.learn.console
import com.arun.learn.Runtime

trait BusinessLogic:
  def doesGoogleHaveEvenAmountOfPictures(topic: String): Boolean

object BusinessLogic:
  lazy val live: ZIO[Google, Nothing, BusinessLogic] = //my using this we would not need to pass google to create instance, when using then we can pass
    ZIO.fromFunction(make)

  def make(google: Google): BusinessLogic =
    new:
      override def doesGoogleHaveEvenAmountOfPictures(topic: String): Boolean =
        google.countPicturesOf(topic) % 2 == 0
trait Google:
  def countPicturesOf(topic: String): Int

object GoogleImpl:
  lazy val live: ZIO[Any, Nothing, Google] = // Any is used here just to make live a fxn, and can be ignored when used
    ZIO.succeed(make)
  def make: Google =
    new:
      override def countPicturesOf(topic: String): Int =
        if (topic == "cat") 1337 else 1338

object DependencyGraph:
  lazy val live: ZIO[Any, Nothing, BusinessLogic] =
    for
      google <- GoogleImpl.live
      businessLogicMaker <- BusinessLogic.live.provide(google)
    yield businessLogicMaker

  // lazy val liveOld: Any => BusinessLogic = BusinessLogic.live.compose(GoogleImpl.live) // OR GoogleImpl.live.andThen(BusinessLogic.live)

  lazy val makeOld: BusinessLogic =
    BusinessLogic.make(GoogleImpl.make)

  lazy val make: BusinessLogic =
    val google = GoogleImpl.make
    val businessLogic = BusinessLogic.make(google)

    businessLogic

object RunMain extends scala.App:
  Runtime.default.unsafeRunToSync(program)
  lazy val program =
    for
      businessLogic <- DependencyGraph.live
      _ <- console.putStrLn("-" * 100)
      _ <- console.putStrLn(businessLogic.doesGoogleHaveEvenAmountOfPictures("cat").toString)
      _ <- console.putStrLn(businessLogic.doesGoogleHaveEvenAmountOfPictures("dog").toString)
      _ <- console.putStrLn("-" * 100)
    yield ()
/* We will write this using ZIO:
  val businessLogic = DependencyGraph
    .live
    .apply(()) //benifit of using live as fxn:  we don't need to create instance at one place

  println("-" * 100)
  println(businessLogic.doesGoogleHaveEvenAmountOfPictures("cat"))
  println(businessLogic.doesGoogleHaveEvenAmountOfPictures("dog"))
  println("-" * 100)

extension [R, A](run: R => A)
  def provide(r: => R): Any => A =
    _ => run(r)

def succeed[A](a: => A): Any => A =
  _ => a

def fromFunction[R, A](run: R => A): R => A =
  r => run(r)
 */
