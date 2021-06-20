package com.arun.learn

object OurZio:
  type Thunk[A] = () => A

  // final case class ZIO[A](thunk: Thunk[A]) both are same. avoid type becuase we are going to use it once
  final case class ZIO[A](thunk: () => A):
    def flatMap[B](azb: A => ZIO[B]): ZIO[B] =
      ZIO.succeed {
        val a = thunk()
        val zb = azb(a)
        val b = zb.thunk()
        b
      }
    def map[B](ab: A => B): ZIO[B] =
      ZIO.succeed {
        val a = thunk()
        val b = ab(a)
        b
      }
  end ZIO

  object ZIO:
    def succeed[A](a: => A): ZIO[A] =
      ZIO(() => a)

  object console:
    def putStrLn(line: String) =
      ZIO.succeed(println(s"$line"))

    val getStrLn =
      ZIO.succeed(scala.io.StdIn.readLine())

  object Runtime:
    object default:
      def unsafeRunToSync[A](zio: => ZIO[A]): A =
        zio.thunk()
