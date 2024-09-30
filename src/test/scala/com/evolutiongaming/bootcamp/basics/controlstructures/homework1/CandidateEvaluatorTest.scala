package com.evolutiongaming.bootcamp.basics.controlstructures.homework1

import dev.codescreen.ControlStructuresHomework1.isValidCandidate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
class CandidateEvaluatorTest extends AnyFlatSpec with Matchers {
  "isValidCandidate" should "return true for a candidate with max experience, education, Wakanda nationality, and enough stars" in {
    isValidCandidate("Wakanda", 6, 5, true, 100) shouldBe true
  }
  it should "return false for a candidate with less than 5 passed tests" in {
    isValidCandidate("Wakanda", 4, 5, true, 100) shouldBe false
  }
  it should "return true for a candidate with no education but enough experience, passed tests, and GitHub stars" in {
    isValidCandidate("Wakanda", 7, 5, false, 100) shouldBe true
  }
  it should "return false for a candidate who has less than 5 years of experience, no education, and no stars" in {
    isValidCandidate("Amestris", 7, 2, false, 0) shouldBe false
  }
  it should "return true for a candidate with no GitHub stars but has passed enough tests and is from Wakanda" in {
    isValidCandidate("Wakanda", 10, 5, true, 0) shouldBe true
  }
  it should "return true for a candidate from Narnia with education and passed tests but no stars" in {
    isValidCandidate("Narnia", 8, 3, true, 0) shouldBe true
  }
  it should "return false for a candidate from a non-preferred country with less than required points" in {
    isValidCandidate("UnknownCountry", 6, 2, false, 5) shouldBe false
  }
  it should "return false for a candidate with zero GitHub stars, no education, and only 6 passed tests" in {
    isValidCandidate("Skyrim", 6, 4, false, 0) shouldBe false
  }
  it should "return true for a candidate from Amestris with max experience and enough passed tests but no stars" in {
    isValidCandidate("Amestris", 7, 5, false, 0) shouldBe false
  }
  it should "return true for a candidate with many GitHub stars, experience, and passed tests" in {
    isValidCandidate("UnknownCountry", 6, 5, false, 1000) shouldBe false
  }
  it should "return false for a candidate from Wakanda with too few tests passed" in {
    isValidCandidate("Wakanda", 3, 5, true, 500) shouldBe false
  }
  it should "return true for a candidate from Narnia with sufficient GitHub stars and experience" in {
    isValidCandidate("Narnia", 6, 3, false, 100) shouldBe false
  }
  it should "return false for a candidate with insufficient stars and only 5 passed tests" in {
    isValidCandidate("Amestris", 5, 5, true, 9) shouldBe false
  }
  it should "return true for a candidate with just enough points to be valid" in {
    isValidCandidate("Wakanda", 6, 5, false, 0) shouldBe false
  }
}
