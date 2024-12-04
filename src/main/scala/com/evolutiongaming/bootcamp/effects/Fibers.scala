package com.evolutiongaming.bootcamp.effects

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import scala.concurrent.duration._
import scala.language.postfixOps

// start
// join
// not swallow error
// cancel

object Fibers extends IOApp {
  val N = 100

  def printMany(string: String): IO[Unit] =
    List.fill(N)(IO.delay(println(string))).sequence.void *> IO.raiseError(new Exception("Uuuups!"))

  val program: IO[Unit] =
    for {
      fiber1 <- printMany("a").start
      fiber2 <- printMany("   b").start
      //_ <- List(printMany("a"), printMany("   b")).parSequence

      _ <- fiber1.cancel

      _ <- fiber1.join.flatMap(_.embedError)
      _ <- fiber2.join.flatMap(_.embedError)

      _ <- IO.delay(println("SUCCESS"))
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
}
