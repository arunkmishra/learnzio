package com.arun.learn

// type Thunk[A] = () => A

// final case class ZIO[A](thunk: Thunk[A]) both are same. avoid type becuase we are going to use it once
final case class ZIO[-R, +E, A](run: R => Either[E, A]):
  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO(r => run(r).fold(ZIO.fail, azb).run(r))

  def map[B](ab: A => B): ZIO[R, E, B] =
    ZIO(r => run(r).map(ab))

  //flatMap for error channel
  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    ZIO(r => run(r).fold(h, ZIO.succeed).run(r))

  //map for error channel
  def mapError[E2](h: E => E2): ZIO[R, E2, A] =
    ZIO(r => run(r).left.map(h))

  def provide(r: => R): ZIO[Any, E, A] =
    ZIO(_ => run(r))
end ZIO

object ZIO:
  def succeed[A](a: => A): ZIO[Any, Nothing, A] =
    ZIO(_ => Right(a))

  def fail[E](e: => E): ZIO[Any, E, Nothing] =
    ZIO(_ => Left(e))

  def effect[A](a: => A): ZIO[Any, Throwable, A] =
    ZIO(_ =>
      try Right(a)
      catch Left(_)
    ) // or ex => Left(ex)

  def fromFunction[R, A](run: R => A): ZIO[R, Nothing, A] =
    ZIO(r => Right(run(r)))

  inline def access[R]: AccessPartiallyApplied[R] =
    AccessPartiallyApplied()

  final class AccessPartiallyApplied[R]():
    def apply[A](f: R => A): ZIO[R, Nothing, A] =
      environment[R].map(f)

  inline def accessM[R]: AccessMPartiallyApplied[R] = //M for monad so that flatmap can be used
    AccessMPartiallyApplied()

  final class AccessMPartiallyApplied[R]():
    def apply[E, A](f: R => ZIO[R, E, A]) =
      environment.flatMap(f)

  inline def environment[R]: ZIO[R, Nothing, R] =
    identity
  //not part of actual ZIO
  inline def read[R]: ZIO[R, Nothing, R] =
    identity

  def identity[R]: ZIO[R, Nothing, R] =
    ZIO.fromFunction(Predef.identity)

object console:
  trait Console:
    def putStrLn(line: String): ZIO[Any, Nothing, Unit]
    def getStrLn: ZIO[Any, Nothing, String]
  object Console:
    lazy val live: ZIO[Any, Nothing, Console] =
      ZIO.succeed(make)
    lazy val make: Console =
      new:
        def putStrLn(line: String) =
          ZIO.succeed(println(s"$line"))
        lazy val getStrLn =
          ZIO.succeed(scala.io.StdIn.readLine())

  def putStrLn(line: String): ZIO[Console, Nothing, Unit] =
    ZIO.accessM(_.putStrLn(line))

  def getStrLn: ZIO[Console, Nothing, String] =
    ZIO.accessM(_.getStrLn)

object Runtime:
  object default:
    def unsafeRunSync[E, A](zio: => ZIO[ZEnv, E, A]): Either[E, A] =
      zio.run((console.Console.make))

type ZEnv = console.Console
