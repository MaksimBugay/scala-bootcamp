package dev.codescreen

import scala.io.Source

object ControlStructuresHomework1 {
  type Error = String

  // consider person an adult if their age is higher or equal to 18
  // return error message "$age is too high, are you human?" if age is higher than 150
  // return error message "$age is negative, we do not serve unborn people" if age is lower than 0
  // use if-else
  def isAdultIf(age: Int): Either[Error, Boolean] =
    if (age > 150)
      Left(s"$age is too high, are you human?")
    else if (age < 0)
      Left(s"$age is negative, we do not serve unborn people")
    else if (age < 18)
      Right(false)
    else Right(true)

  // same as isAdultIf, but use match statement instead
  def isAdultMatch(age: Int): Either[Error, Boolean] = age match {
    case age if age > 150 => Left(s"$age is too high, are you human?")
    case age if age < 0   => Left(s"$age is negative, we do not serve unborn people")
    case age if age < 18  => Right(false)
    case _                => Right(true)
  }

  trait Triangle {
    def a: Double
    def b: Double
    def c: Double

        // Common methods for all triangles
    def perimeter: Double                                         = a + b + c
    def area: Double                                              = {
      val s = perimeter / 2 // Semi-perimeter
      Math.sqrt(s * (s - a) * (s - b) * (s - c)) // Heron's formula
    }
  }

  // Define the ValidTriangle case class
  case class ValidTriangle private (a: Double, b: Double, c: Double) extends Triangle

  // Companion object to handle instantiation and validation
  object ValidTriangle {
    def apply(a: Double, b: Double, c: Double): ValidTriangle = {
      require(a > 0 && b > 0 && c > 0, "Sides must be positive")
      require(isValidTriangleF(a, b, c), s"Invalid triangle with sides $a, $b, $c")
      new ValidTriangle(a, b, c) // Create the valid triangle
    }

    private def isValidTriangleF(a: Double, b: Double, c: Double): Boolean =
      a + b > c && a + c > b && b + c > a

    def create(a: Double, b: Double, c: Double): Either[Error, ValidTriangle] = {
      try {
        val triangle = ValidTriangle(a, b, c)
        Right(triangle)
      } catch {
        case e: IllegalArgumentException => Left(s"${e.getMessage}")
      }
    }
  }
  // https://en.wikipedia.org/wiki/Triangle_inequality, consider degenerate triangles invalid
  // can you do it without using any control structures?
  def isValidTriangle(a: Double, b: Double, c: Double): Boolean = ValidTriangle.create(a, b, c) match {
    case Left(_) => false
    case Right(_) => true
  }

  // IT company located in Wakanda is searching for a new programmer. Due to high interest it needs
  // a way to filter out candidates that are not suitable for this job.
  // They have a number of characteristics that give candidates points, and valid candidate
  // should earn at least 10 points.
  //
  // * Each year of experience gives candidate 1 point, but not more than 5
  // * If candidate has education, it gives him 3 points
  // * Each passed test gives candidate 1 point starting from 6th passed test.
  //   If candidate has passed less than 5 tests, they don't want to hire him in any case.
  // * They prefer candidates from their country, so being from Wakanda gives candidate 3 points.
  //   If the candidate is from any of the neighboring countries - Narnia, Skyrim or Amestris -
  //   candidate should get 1 point, otherwise - 0
  // * Summed stars on github also give points.
  //   1 point if candidate has 10 stars, 2 points for 100, 3 points for 1000 and so on,
  //   giving 1 point for each WHOLE new power of ten, e.g. 9 is still 0 points, 99 is still 1 point.
  //
  // All input is valid, e.g. candidate can't have negative years of experience

  case class Candidate(
                        country: String,
                        passedTests: Int,
                        yearsOfExperience: Int,
                        hasEducation: Boolean,
                        starsOnGithub: Int
                      ) {
    // Validation to ensure non-negative values
    require(passedTests >= 0, "passedTests cannot be negative")
    require(yearsOfExperience >= 0, "yearsOfExperience cannot be negative")
    require(starsOnGithub >= 0, "starsOnGithub cannot be negative")
  }

  object CandidateEvaluator {
    // Calculate points for years of experience (max 5 points)
    private def experiencePoints(candidate: Candidate): Int =
      math.min(candidate.yearsOfExperience, 5)
    // 3 points for education, 0 if no education
    private def educationPoints(candidate: Candidate): Int =
      if (candidate.hasEducation) 3 else 0
    // Points for passed tests (if fewer than 5 tests, candidate is invalid)
    private def testPoints(candidate: Candidate): Either[String, Int] = {
      if (candidate.passedTests < 5) Left("Candidate must pass at least 5 tests")
      else Right(math.max(0, candidate.passedTests - 5))
    }
    // Points for country of origin (3 points for Wakanda, 1 for neighboring countries, 0 otherwise)
    private def countryPoints(candidate: Candidate): Int = candidate.country match {
      case "Wakanda" => 3
      case "Narnia" | "Skyrim" | "Amestris" => 1
      case _ => 0
    }
    // Points based on GitHub stars (1 point for every power of 10)
    private def githubPoints(candidate: Candidate): Int =
      if (candidate.starsOnGithub == 0) 0 else math.log10(candidate.starsOnGithub.toDouble).toInt

    // Check if the candidate is valid by summing up all the points
    def isValid(candidate: Candidate): Boolean = {
      val totalPoints = for {
        testPts <- testPoints(candidate)  // Calculate test points (if valid)
      } yield {
        experiencePoints(candidate) +
          educationPoints(candidate) +
          testPts +
          countryPoints(candidate) +
          githubPoints(candidate)
      }
      // A candidate is valid if the total points are at least 10
      totalPoints match {
        case Right(points) => points >= 10
        case Left(_) => false  // Invalid candidate due to not passing enough tests
      }
    }
  }

  def isValidCandidate(
                        country: String,
                        passedTests: Int,
                        yearsOfExperience: Int,
                        hasEducation: Boolean,
                        starsOnGithub: Int,
                      ): Boolean = {
    try {
      val candidate = Candidate(
        country,
        passedTests,
        yearsOfExperience,
        hasEducation,
        starsOnGithub
      )
      CandidateEvaluator.isValid(candidate)
    } catch {
      case e: IllegalArgumentException => false
    }
  }

}
