package com.arun.learn.others

import com.arun.learn.*
// import zio.*

object businessLogic:
  trait BusinessLogic:
    def doesGoogleHaveEvenAmountOfPictures(topic: String): ZIO[Any, Nothing, Boolean]

  object BusinessLogic:
    lazy val live: ZIO[Google, Nothing, BusinessLogic] = //my using this we would not need to pass google to create instance, when using then we can pass
      ZIO.fromFunction(make)
    def make(google: Google): BusinessLogic =
      new:
        override def doesGoogleHaveEvenAmountOfPictures(topic: String): ZIO[Any, Nothing, Boolean] =
          google.countPicturesOf(topic).map(_ % 2 == 0)

  def doesGoogleHaveEvenAmountOfPictures(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM[BusinessLogic](_.doesGoogleHaveEvenAmountOfPictures(topic))

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
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic] =
    for
      g <- GoogleImpl.live
      bl <- businessLogic.BusinessLogic.live.provide(g)
    yield bl

  // lazy val liveOld: Any => BusinessLogic = BusinessLogic.live.compose(GoogleImpl.live) // OR GoogleImpl.live.andThen(BusinessLogic.live)

  lazy val makeOld: businessLogic.BusinessLogic =
    businessLogic.BusinessLogic.make(GoogleImpl.make)

  lazy val make: businessLogic.BusinessLogic =
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
      p <- makeProgram.provideSome[Has[ZEnv]](_ union Has(bl))
    yield p
//problem here is: code doesnt know how to pass console
// solution : use cake pattern to provide console
  lazy val makeProgram =
    for
      //bl <- ZIO.environment
      // ZIO.fromFunction[BusinessLogic, BusinessLogic](identity)
      // ZIO.fromFunction((r: BusinessLogic) => r)
      env <- ZIO.environment[Has[console.Console] & Has[businessLogic.BusinessLogic]]
      _ <- env.get[console.Console].putStrLn("-" * 100)
      cats <- env.get[businessLogic.BusinessLogic].doesGoogleHaveEvenAmountOfPictures("cat")
      _ <- env.get[console.Console].putStrLn(cats.toString)
      dogs <- env.get[businessLogic.BusinessLogic].doesGoogleHaveEvenAmountOfPictures("dog")
      //map removed by access
      //dogs <- ZIO.access[BusinessLogic].map(_.doesGoogleHaveEvenAmountOfPictures("dog"))
      _ <- env.get[console.Console].putStrLn(dogs.toString)

      _ <- env.get[console.Console].putStrLn("-" * 100)
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
