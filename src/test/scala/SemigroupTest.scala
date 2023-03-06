import org.scalatest.funsuite.AnyFunSuite

import cats.implicits._
import cats.Semigroup
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._

class SemigroupTest extends AnyFunSpec with Matchers {

  // A Semigroup for a given type A has a main operation called combine.
  // This operation takes two values of type A and returns a value of type A.
  // The operation must be guaranteed to be associative, this means that 
  // ((a combine b) combine c) == (a combine (b combine c))
  // for all posible values of a, b, c

  describe("Semigroup of Int") {
    describe("When combining 1 and 2") {
      it("Should result in 3") {
        Semigroup[Int].combine(1, 2) should be (3)
      }
    }
  }

  describe("Semigroup of List of Int") {
    describe("When combining List with 1,2,3 and List with 4,5,6") {
      it("Should result in List with 1,2,3,4,5,6") {
        Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6)) should be (List(1,2,3,4,5,6))
      }
    }
  }

  describe("Semigroup of Option of Int") {
    describe("When combining Option of 1 and Option of 2") {
      it("Should result in Option of 3") {
        Semigroup[Option[Int]].combine(Option(1), Option(2)) should be (Some(3))
      }
    }

    describe("When combining Option of 1 and None") {
      it("Should result in Option of 1") {
        Semigroup[Option[Int]].combine(Option(1), None) should be (Some(1))
      }
    }

    describe("When combining None and None") {
      it("Should result in None") {
        Semigroup[Option[Int]].combine(None, None) should be (None)
      }
    }
  }

  describe("Semigroup of a String") {
    describe("When combining string \"Hello\" and \" World!\"") {
      it("Should result in \"Hello World!\""){
        Semigroup[String].combine("Hello", " World!") should be ("Hello World!")
      }
    }
  }

  describe("Semigroup of a function that receives an Int and returns an Int") {
    describe("When combining two functions one that sums and the other that multiplies") {
      it("Should result in the sum of the result of each function") {
        Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(4) should be (45)
      }
    }
  }

  describe("Semigroup of a Map") {
    describe("When combining map1 Map(\"hello\" -> 1, \"world\" -> 1) with Map(\"hello\" -> 2, \"cats\"  -> 3)") {
      it("Should result in Map(\"hello\" -> 3, \"cats\" -> 3, \"world\" -> 1)") {
        Semigroup[Map[String, Int]].combine(Map("hello" -> 1, "world" -> 1), Map("hello" -> 2, "cats"  -> 3)) should be (Map("hello" -> 3, "cats" -> 3, "world" -> 1))
      }
    }
  }

}