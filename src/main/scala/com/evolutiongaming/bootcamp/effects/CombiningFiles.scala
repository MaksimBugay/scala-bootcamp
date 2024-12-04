package com.evolutiongaming.bootcamp.effects

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.effect.implicits._
import cats.implicits._
import com.evolutiongaming.bootcamp.effects.CombiningFilesIO.{printLine, readAll}

import java.io.{Closeable, FileNotFoundException, PrintWriter}
import scala.io.{BufferedSource, Source}

// Revision:

// delay vs pure
// TF is like imports
// raise error is like throw an exception

object CombiningFiles extends App {
  println("Started")

  val input1 = Source.fromFile("file1.txt")
  val input2 = Source.fromFile("file2.txt")
  val output = new PrintWriter("file3.txt")

  input1.getLines().foreach(output.println)
  input2.getLines().foreach(output.println)

  input1.close()
  input2.close()
  output.close()

  println("Finished")
}

object CombiningFilesIO extends IOApp {
  private def openInput(filename: String): IO[BufferedSource] =
    IO.blocking(Source.fromFile(filename))

  private def openClassPathInput(filename: String): IO[BufferedSource] =
    IO.blocking {
      val resource = getClass.getClassLoader.getResource(filename)
      if (resource != null) {
        Source.fromURL(resource)
      } else {
        throw new FileNotFoundException(s"Resource $filename not found")
      }
    }

  private def openOutput(filename: String): IO[PrintWriter] =
    IO.blocking(new PrintWriter(filename))

  def readAll(input: BufferedSource): IO[List[String]] =
    IO.blocking(input.getLines().toList)

  def printLine(output: PrintWriter, string: String): IO[Unit] =
    IO.blocking(output.println(string))

  def close(closable: Closeable): IO[Unit] =
    IO.blocking(closable.close())

  private def closeWithErrorHandling(resource: Closeable): IO[Unit] =
    IO.blocking(resource.close()).handleErrorWith(_ => IO.unit)

  val files: List[String] = List("file1.txt", "file2.txt", "file3.txt")

  val program = for {
    _ <- IO.delay(println("Started"))

    _ <- IO.fromEither(123.asRight[Throwable])

    inputFiles <- files.traverse(openInput)

    lines <- inputFiles.traverse(readAll).map(_.flatten)

    outputWriter <- openOutput("result.txt")

    _ <- lines.traverse_(printLine(outputWriter, _))

    _ <- close(outputWriter)

    _ <- inputFiles.traverse_(closeWithErrorHandling)

    _ <- IO.delay(println("Finished"))
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
}

object CombiningFilesResource extends IOApp {
  def openInput(filename: String): Resource[IO, BufferedSource] =
    Resource.make(IO.blocking(Source.fromFile(filename)))(r => IO.blocking(r.close()))

  def openOutput(filename: String): Resource[IO, PrintWriter] =
    Resource.make(IO.blocking(new PrintWriter(filename)))(r => IO.blocking(r.close()))

  def readAll(input: BufferedSource): IO[List[String]] =
    IO.blocking(input.getLines().toList)

  def printLine(output: PrintWriter, string: String): IO[Unit] =
    IO.blocking(output.println(string))

  val files: Resource[IO, (BufferedSource, BufferedSource, PrintWriter)] = for {
    input1 <- openInput("file1.txt")
    input2 <- openInput("file2.txt")
    output <- openOutput("result.txt")
  } yield (input1, input2, output)

  // different monads in for
  /*
    val program: IO[Unit] = {
      for {
        _ <- IO.delay(println("Started"))
        _ <- files.use { case (input1, input2, output) =>
          for {
            lines <- List(input1, input2).traverse(readAll).map(_.flatten)
            _ <- lines.traverse_(line => printLine(output, line))
          } yield ()
        }
        _ <- IO.delay(println("Finished"))
      } yield ()
    }
  */
  val fileNames: List[String] = List("file1.txt", "file2.txt")
  val program: IO[Unit] = {
    val res: Resource[IO, Unit] = for {
      _ <- Resource.eval(IO.delay(println("Started")))
      inputFiles <- fileNames.traverse(openInput)
      outputWriter <- openOutput("result.txt")
      _ <- Resource.eval {
        for {
          lines <- inputFiles.traverse(readAll).map(_.flatten)
          _ <- lines.traverse_(printLine(outputWriter, _))
        } yield ()
      }
      _ <- Resource.eval(IO.delay(println("Finished")))
    } yield ()

    res.use(_ => IO.unit)
  }

  val program2: IO[Unit] = {
    val res: Resource[IO, Unit] = for {
      _ <- Resource.eval(IO.delay(println("Started")))
      file1 <- openInput("file1.txt")
      file2 <- openInput("file2.txt")
      outputWriter <- openOutput("result.txt")

      content1Fiber <- Resource.eval(readAll(file1).start)
      content2Fiber <- Resource.eval(readAll(file2).start)

      content1 <- Resource.eval(content1Fiber.join.flatMap(_.embedError))
      content2 <- Resource.eval(content2Fiber.join.flatMap(_.embedError))

      _ <- Resource.eval(content1.traverse(l => printLine(outputWriter, l)))
      _ <- Resource.eval(content2.traverse(l => printLine(outputWriter, l)))

      _ <- Resource.eval(IO.delay(println("Finished")))
    } yield ()

    res.use(_ => IO.unit)
  }
  override def run(args: List[String]): IO[ExitCode] = program2.as(ExitCode.Success)
}

// try fibers
