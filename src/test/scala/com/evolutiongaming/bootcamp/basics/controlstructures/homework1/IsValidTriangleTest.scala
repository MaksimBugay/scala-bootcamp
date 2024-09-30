package com.evolutiongaming.bootcamp.basics.controlstructures.homework1

import dev.codescreen.ControlStructuresHomework1.isValidTriangle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IsValidTriangleTest extends AnyFlatSpec with Matchers {
  "isValidTriangle" should "return true for a valid triangle" in {
    isValidTriangle(3.0, 4.0, 5.0) shouldBe true  // Valid triangle
  }
  it should "return false for sides that violate the triangle inequality" in {
    isValidTriangle(1.0, 2.0, 10.0) shouldBe false  // Sum of two sides <= third side
  }
  it should "return false for sides with zero length" in {
    isValidTriangle(0.0, 4.0, 5.0) shouldBe false  // Invalid triangle
  }
  it should "return false for negative side lengths" in {
    isValidTriangle(-3.0, 4.0, 5.0) shouldBe false  // Invalid triangle due to negative side
  }
  it should "return false for a degenerate triangle (where two sides equal the third)" in {
    isValidTriangle(1.0, 1.0, 2.0) shouldBe false  // Degenerate triangle
  }
  it should "return true for a triangle with sides just satisfying the inequality" in {
    isValidTriangle(5.0, 5.0, 5.0) shouldBe true  // Valid equilateral triangle
  }
  it should "return false for a triangle with very large sides that are invalid" in {
    isValidTriangle(1e20, 1e20, 1e40) shouldBe false  // Violates triangle inequality
  }
  it should "return true for a triangle with very large valid sides" in {
    isValidTriangle(1e20, 1e20, 1e20) shouldBe true  // Large but valid triangle
  }
}
