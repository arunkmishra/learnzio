package com.arun.learn.others

import com.arun.learn.*
// import zio.*

object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  object BusinessLogic:
    trait Service:
      def doesGoogleHaveEvenAmountOfPictures(topic: String): ZIO[Any, Nothing, Boolean]

    lazy val live: ZIO[Google, Nothing, Service] = //my using this we would not need to pass google to create instance, when using then we can pass
      ZIO.fromFunction(make)
    def make(google: Google): Service =
      new:
        override def doesGoogleHaveEvenAmountOfPictures(topic: String): ZIO[Any, Nothing, Boolean] =
          google.countPicturesOf(topic).map(_ % 2 == 0)

  def doesGoogleHaveEvenAmountOfPictures(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.doesGoogleHaveEvenAmountOfPictures(topic))

trait Google:
  def countPicturesOf(topic: String): ZIO[Any, Nothing, Int]

object GoogleImpl:
  lazy val live: ZIO[Any, Nothing, Google] = // Any is used here just to make live a fxn, and can be ignored when used
    ZIO.succeed(make)
  def make: Google =
    new:
      override def countPicturesOf(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if (topic == "cat") 1337 else 1338)

object DependencyGraph:
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic.Service] =
    for
      g <- GoogleImpl.live
      bl <- businessLogic.BusinessLogic.live.provide(g)
    yield bl

  // lazy val liveOld: Any => BusinessLogic = BusinessLogic.live.compose(GoogleImpl.live) // OR GoogleImpl.live.andThen(BusinessLogic.live)

  lazy val makeOld: businessLogic.BusinessLogic.Service =
    businessLogic.BusinessLogic.make(GoogleImpl.make)

  lazy val make: businessLogic.BusinessLogic.Service =
    val g = GoogleImpl.make
    val bl = businessLogic.BusinessLogic.make(g)
    bl

object RunMain extends scala.App:
  // commented in favour of runtime on line 70
  // Runtime.default.unsafeRunToSync(program)
  /*
problem with this approach is we cannot inject dependency
lazy val program =
    for
      businessLogic <- DependencyGraph.live
      _ <- console.putStrLn("-" * 100)
      _ <- console.putStrLn(businessLogic.doesGoogleHaveEvenAmountOfPictures("cat").toString)
      _ <- console.putStrLn(businessLogic.doesGoogleHaveEvenAmountOfPictures("dog").toString)
      _ <- console.putStrLn("-" * 100)
    yield ()*/
// to pass dependency using argument we can do something like this
  /*lazy val program =
    for {
      businessLogic <- DependencyGraph.live
      p <- makeProgram(businessLogic)
    } yield p

  def makeProgram(businessLogic: BusinessLogic) =*/
// now we want to do above thing using reader way. using fromFunction
  Runtime.default.unsafeRunSync(program)
  lazy val program =
    for
      bl <- DependencyGraph.live
      p <- makeProgram.provideCustomLayer(Has(bl)) // makeProgram.provideSome[ZEnv](_ union Has(bl))
    yield p
//problem here is: code doesnt know how to pass console
// solution : use cake pattern to provide console
  lazy val makeProgram =
    for
      //bl <- ZIO.environment
      // ZIO.fromFunction[BusinessLogic, BusinessLogic](identity)
      // ZIO.fromFunction((r: BusinessLogic) => r)
      _ <- console.putStrLn("-" * 100)
      cats <- businessLogic.doesGoogleHaveEvenAmountOfPictures("cat")
      _ <- console.putStrLn(cats.toString)
      dogs <- businessLogic.doesGoogleHaveEvenAmountOfPictures("dog")
      //map removed by access
      //dogs <- ZIO.access[BusinessLogic].map(_.doesGoogleHaveEvenAmountOfPictures("dog"))
      _ <- console.putStrLn(dogs.toString)

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
