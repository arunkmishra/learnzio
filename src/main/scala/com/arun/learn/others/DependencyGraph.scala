package com.arun.learn.others

trait BusinessLogic:
  def doesGoogleHaveEvenAmountOfPictures(topic: String): Boolean

object BusinessLogic:
  def make(google: Google): BusinessLogic =
    new:
      override def doesGoogleHaveEvenAmountOfPictures(topic: String): Boolean =
        google.countPicturesOf(topic) % 2 == 0
trait Google:
  def countPicturesOf(topic: String): Int

object GoogleImpl:
  def make: Google =
    new:
      override def countPicturesOf(topic: String): Int =
        if (topic == "cat") 1337 else 1338

object DependencyGraph:
  lazy val make: BusinessLogic =
    val google = GoogleImpl.make
    val businessLogic = BusinessLogic.make(google)

    businessLogic

object RunMain extends scala.App:

  val businessLogic = DependencyGraph.make

  println("-" * 100)
  println(businessLogic.doesGoogleHaveEvenAmountOfPictures("cat"))
  println(businessLogic.doesGoogleHaveEvenAmountOfPictures("dog"))
  println("-" * 100)
