package com.arun.learn

import OurZio.*

object Main extends scala.App:

  Runtime.default.unsafeRunToSync(program) // make program lazy val to avid forward ref error

  lazy val trace = s"[${Console.BLUE}TRACE ${Console.RESET}]"

  lazy val program =
    for
      _ <- console.putStrLn("-" * 100)
      _ <- console.putStrLn("What is your name?") // .debug(trace)
      name <- ZIO.succeed("Arun Kumar")
      _ <- console.putStrLn(s"Hello $name")

      // _ <- ZIO.fail("BOOM") //throw error
      //  _ <- ZIO.effect(throw RuntimeException("boom")) // wont throw exception : by name parameter
      _ <- console.putStrLn("-" * 100)
    yield ()

// Runtime.default.unsafeRunToFuture(program)
