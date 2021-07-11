package com.arun.learn

// type Thunk[A] = () => A

// final case class ZIO[A](thunk: Thunk[A]) both are same. avoid type becuase we are going to use it once
final case class ZIO[+E, A](thunk: () => Either[E, A]):
  def flatMap[E1 >: E, B](azb: A => ZIO[E1, B]): ZIO[E1, B] =
    ZIO { () =>
      val errorOrA = thunk()
      // val zErrorOrB = errorOrA.fold(ZIO.fail, azb) below line replacement
      val zErrorOrB = errorOrA match
        case Right(a) => azb(a)
        case Left(e)  => ZIO.fail(e)
      val b = zErrorOrB.thunk()
      b
    }
  def map[B](ab: A => B): ZIO[E, B] =
    ZIO { () =>
      val errorOrA = thunk()
      val errorOrB = errorOrA match
        case Right(a) => Right(ab(a))
        case Left(e)  => Left(e)
      errorOrB
    }

  //flatMap for error channel
  def catchAll[E2, A1 >: A](h: E => ZIO[E2, A1]): ZIO[E2, A1] =
    ZIO { () =>
      val errorOrA = thunk()
      val zErrorOrB = errorOrA match //instead use fold to make it less verbose
        case Right(a) => ZIO.succeed(a)
        case Left(e)  => h(e)
      val b = zErrorOrB.thunk()
      b
    }
  //map for error channel
  def mapError[E2](h: E => E2): ZIO[E2, A] =
    ZIO { () =>
      val errorOrA = thunk()
      val errorOrB = errorOrA match
        case Right(a) => Right(a)
        case Left(e)  => Left(h(e))

      errorOrB
    }
end ZIO

object ZIO:
  def succeed[A](a: => A): ZIO[Nothing, A] =
    ZIO(() => Right(a))

  def fail[E](e: => E): ZIO[E, Nothing] =
    ZIO(() => Left(e))

  def effect[A](a: => A): ZIO[Throwable, A] =
    ZIO(() =>
      try Right(a)
      catch Left(_)
    ) // or ex => Left(ex)

object console:
  def putStrLn(line: String) =
    ZIO.succeed(println(s"$line"))

  val getStrLn =
    ZIO.succeed(scala.io.StdIn.readLine())

object Runtime:
  object default:
    def unsafeRunToSync[E, A](zio: => ZIO[E, A]): Either[E, A] =
      zio.thunk()
