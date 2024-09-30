package com.evolutiongaming.bootcamp.basics.controlstructures.homework1

import dev.codescreen.ControlStructuresHomework1
import dev.codescreen.ControlStructuresHomework1.isAdultMatch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
class IsAdultTest extends AnyFlatSpec with Matchers {
  // Define a function type to be passed into the tests
  type IsAdultFunc = Int => Either[String, Boolean]
  // Common test suite for both implementations
  def commonTests(isAdult: IsAdultFunc): Unit = {
    it should "return error message if age is higher than 150" in {
      val result = isAdult(160)
      result shouldBe Left("160 is too high, are you human?")
    }
    it should "return error message if age is negative" in {
      val result = isAdult(-5)
      result shouldBe Left("-5 is negative, we do not serve unborn people")
    }
    it should "return false if age is less than 18" in {
      val result = isAdult(10)
      result shouldBe Right(false)
    }
    it should "return true if age is 18" in {
      val result = isAdult(18)
      result shouldBe Right(true)
    }
    it should "return true if age is between 18 and 150" in {
      val result = isAdult(30)
      result shouldBe Right(true)
    }
    it should "return true if age is exactly 150" in {
      val result = isAdult(150)
      result shouldBe Right(true)
    }
  }
  // Tests for isAdultIf
  "isAdultIf" should behave like commonTests(ControlStructuresHomework1.isAdultIf)
  // Tests for isAdultMatch
  "isAdultMatch" should behave like commonTests(isAdultMatch)
}
