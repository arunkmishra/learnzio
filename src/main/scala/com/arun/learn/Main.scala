package com.arun.learn

import OurZio.*

object Main extends scala.App:
//println to see the exit code
  println(
    Runtime.default.unsafeRunToSync(program)
  ) // make program lazy val to avid forward ref error

  lazy val trace = s"[${Console.BLUE}TRACE ${Console.RESET}]"

  lazy val program =
    for
      _ <- console.putStrLn("-" * 100)
      _ <- console.putStrLn("What is your name?") // .debug(trace)
      name <- ZIO.succeed("Arun Kumar")
      // _ <- ZIO.fail(RuntimeException("boom")) // type of program: ZIO[RuntimeExcpetion, Unit
      _ <- ZIO
        .effect(throw RuntimeException("boom")) // type of program: ZIO[Throwable,Unit]
        // .catchAll(_ => ZIO.succeed("ignore error")) //type of program: ZIO[Nothing, unit]
        .mapError(_.getMessage) //type of program: ZIO[String, unit]
      _ <- console.putStrLn(s"Hello $name")

      // _ <- ZIO.fail("BOOM") //throw error
      //  _ <- ZIO.effect(throw RuntimeException("boom")) // wont throw exception : by name parameter
      _ <- console.putStrLn("-" * 100)
    yield ()

// Runtime.default.unsafeRunToFuture(program)

/*
 * notes:
 * after adding error handlers ZIO[E,A] we cannot execute the code, we want to add variance to fix the issue.
 * to see error remove the variance.
 * _ <- console.putStrLn(s"Hello $name")
[error]    |      ^
[error]    |      Found:    com.arun.learn.OurZio.ZIO[Nothing, Unit]
[error]    |      Required: com.arun.learn.OurZio.ZIO[String, Nothing]
 * - to see the fix go see variance in another file
 * */
