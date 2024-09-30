package com.evolutiongaming.bootcamp.basics
import scala.io.Source
object ControlStructuresHomework2 {
  // Define all possible commands
  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double])                extends Command
    final case class Average(numbers: List[Double])            extends Command
    final case class Min(numbers: List[Double])                extends Command
    final case class Max(numbers: List[Double])                extends Command
  }
  final case class ErrorMessage(value: String)
  // Result type to represent the final result after evaluating the command
  sealed trait Result
  final case class DivisionResult(dividend: Double, divisor: Double, result: Double) extends Result
  final case class SumResult(numbers: List[Double], result: Double)                 extends Result
  final case class AverageResult(numbers: List[Double], result: Double)             extends Result
  final case class MinResult(numbers: List[Double], result: Double)                 extends Result
  final case class MaxResult(numbers: List[Double], result: Double)                 extends Result
  // Parse the command from the input string
  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val tokens = x.trim.split("\\s+").toList
    tokens match {
      case "divide" :: a :: b :: Nil =>
        for {
          dividend <- parseDouble(a)
          divisor  <- parseDouble(b)
        } yield Command.Divide(dividend, divisor)
      case "sum" :: xs =>
        parseListOfDoubles(xs).map(Command.Sum)
      case "average" :: xs =>
        parseListOfDoubles(xs).map(Command.Average)
      case "min" :: xs =>
        parseListOfDoubles(xs).map(Command.Min)
      case "max" :: xs =>
        parseListOfDoubles(xs).map(Command.Max)
      case _ =>
        Left(ErrorMessage("Unknown command or invalid input format"))
    }
  }
  // Helper to parse a single double value
  def parseDouble(s: String): Either[ErrorMessage, Double] = {
    try Right(s.toDouble)
    catch { case _: NumberFormatException => Left(ErrorMessage(s"Invalid number: $s")) }
  }
  // Helper to parse a list of doubles
  def parseListOfDoubles(xs: List[String]): Either[ErrorMessage, List[Double]] = {
    try {
      val parsed = xs.map(d => parseDouble(d) match {
        case Right(dv) => dv
        case Left(errorMessage: ErrorMessage) => throw new IllegalArgumentException(errorMessage.value)
      })
      Right(parsed)
    } catch {
      case e: IllegalArgumentException => Left(ErrorMessage(e.getMessage))
    }
  }
  // Calculate the result based on the command
  def calculate(cmd: Command): Either[ErrorMessage, Result] = cmd match {
    case Command.Divide(dividend, divisor) =>
      if (divisor == 0) Left(ErrorMessage("Division by zero"))
      else Right(DivisionResult(dividend, divisor, dividend / divisor))
    case Command.Sum(numbers) =>
      Right(SumResult(numbers, numbers.sum))
    case Command.Average(numbers) =>
      if (numbers.isEmpty) Left(ErrorMessage("Cannot calculate the average of an empty list"))
      else Right(AverageResult(numbers, numbers.sum / numbers.size))
    case Command.Min(numbers) =>
      Right(MinResult(numbers, numbers.min))
    case Command.Max(numbers) =>
      Right(MaxResult(numbers, numbers.max))
  }
  // Render the result in a human-readable format
  def renderResult(result: Result): String = result match {
    case DivisionResult(dividend, divisor, res) =>
      s"$dividend divided by $divisor is $res"
    case SumResult(numbers, res) =>
      s"the sum of ${numbers.mkString(" ")} is $res"
    case AverageResult(numbers, res) =>
      s"the average of ${numbers.mkString(" ")} is $res"
    case MinResult(numbers, res) =>
      s"the minimum of ${numbers.mkString(" ")} is $res"
    case MaxResult(numbers, res) =>
      s"the maximum of ${numbers.mkString(" ")} is $res"
  }
  // Process a single input line by parsing the command, calculating the result, and rendering the result
  def process(x: String): String = {
    import cats.implicits._
    // Using a for-comprehension to handle errors gracefully and process the command flow
    (for {
      command <- parseCommand(x)
      result  <- calculate(command)
    } yield renderResult(result)).leftMap(_.value).merge
  }
  // Main method to read commands from stdin and process them
  def main(args: Array[String]): Unit = {
    Source.stdin.getLines() map process foreach println
  }
}