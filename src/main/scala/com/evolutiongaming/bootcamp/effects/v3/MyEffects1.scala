package com.evolutiongaming.bootcamp.effects.v3

import cats.Monad
import cats.effect._
import cats.syntax.all._

trait MyConsole[F[_]] {
  def putString(value: String): F[Unit]

  def readString: F[String]
}

object MyConsole {
  def apply[F[_]](implicit ev: MyConsole[F]): MyConsole[F] = ev
}

object MyConsoleIO extends MyConsole[IO] {
  def putString(value: String): IO[Unit] = IO.pure(println(value))

  def readString: IO[String] = IO.pure(scala.io.StdIn.readLine())
}

class HelloWorld[F[_] : Monad : MyConsole] {
  def nameProgram: F[Unit] = {
    val console = MyConsole[F]
    for {
      _ <- console.putString("What's your name?")
      name <- console.readString
      _ <- console.putString(s"Hi, $name!")
    } yield ()
  }
}

object MyHelloWorldTF extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    implicit val consoleIO: MyConsole[IO] = MyConsoleIO
    val program = new HelloWorld[IO].nameProgram
    program.as(ExitCode.Success)
  }
}
