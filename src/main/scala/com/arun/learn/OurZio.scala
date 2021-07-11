package com.arun.learn

// type Thunk[A] = () => A

// final case class ZIO[A](thunk: Thunk[A]) both are same. avoid type becuase we are going to use it once
final case class ZIO[-R, +E, A](run: R => Either[E, A]):
  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO { r =>
      val errorOrA = run(r)
      // val zErrorOrB = errorOrA.fold(ZIO.fail, azb) below line replacement
      val zErrorOrB = errorOrA match
        case Right(a) => azb(a)
        case Left(e)  => ZIO.fail(e)
      val b = zErrorOrB.run(r)
      b
    }
  def map[B](ab: A => B): ZIO[R, E, B] =
    ZIO { r =>
      val errorOrA = run(r)
      val errorOrB = errorOrA match
        case Right(a) => Right(ab(a))
        case Left(e)  => Left(e)
      errorOrB
    }

  //flatMap for error channel
  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    ZIO { r =>
      val errorOrA = run(r)
      val zErrorOrB = errorOrA match //instead use fold to make it less verbose
        case Right(a) => ZIO.succeed(a)
        case Left(e)  => h(e)
      val b = zErrorOrB.run(r)
      b
    }
  //map for error channel
  def mapError[E2](h: E => E2): ZIO[R, E2, A] =
    ZIO { r =>
      val errorOrA = run(r)
      val errorOrB = errorOrA match
        case Right(a) => Right(a)
        case Left(e)  => Left(h(e))

      errorOrB
    }

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

object console:
  def putStrLn(line: String) =
    ZIO.succeed(println(s"$line"))

  lazy val getStrLn =
    ZIO.succeed(scala.io.StdIn.readLine())

object Runtime:
  object default:
    def unsafeRunToSync[E, A](zio: => ZIO[ZEnv, E, A]): Either[E, A] =
      zio.run(())

type ZEnv = Unit
