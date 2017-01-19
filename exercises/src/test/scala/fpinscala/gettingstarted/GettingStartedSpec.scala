package fpinscala.gettingstarted

import scala.Array.canBuildFrom
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Sorting

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

import MyModule.fib
import PolymorphicFunctions.compose
import PolymorphicFunctions.curry
import PolymorphicFunctions.isSorted
import PolymorphicFunctions.isSortedFirstTry

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class GettingStartedSpec extends FlatSpec with PropertyChecks{

  behavior of "fibonacci"

  it should "work" in {
    val tests = Table(
      ("n", "fib(n)"),
      (0,0), (1,1), (2,1), (3,2), (4,3), (5,5), (6,8), (7,13)
    )
    forAll(tests) { (x: Int, y: Int) =>
      assertResult(y)(fib(x))
    }
  }

  it should "be the sum of the two previous fibonacci numbers" in {
    forAll(Gen.chooseNum(2, 100) :| "n") { n: Int =>
      assertResult(fib(n - 1) + fib(n - 2))(fib(n))
    }
  }

  behavior of "isSorted"

  // use Scala's built-in Orderings
  import scala.math.Ordering.Implicits._
  def gt[A: Ordering](x: A, y: A) = x >= y

  it should "work" in {

    def tableTestFirstTry[A: Ordering](gt: (A, A) => Boolean)(x: Array[A], expected: Boolean): Unit = {
      assertResult(expected)(isSortedFirstTry(x, gt))
    }

    val testsIntFirstTry = Table(
      ("as", "expected"),
      (Array(0, 1), true),
      (Array(0, 1, 2), true),
      (Array(0, 2, 1), false))
    forAll(testsIntFirstTry)(tableTestFirstTry(gt[Int]))

    def tableTest[A: Ordering](gt: (A, A) => Boolean)(x: Array[A], expected: Boolean): Unit = {
      assertResult(expected)(isSorted(x, gt))
    }

    val testsInt = Table(
      ("as", "expected"),
      (Array[Int](), true),
      (Array(0), true),
      (Array(0, 0), true),
      (Array(0, 1), true),
      (Array(0, 1, 2), true),
      (Array(0, 2, 1), false))
    forAll(testsInt)(tableTest(gt[Int]))

    val testsString = Table(
      ("as", "y"),
      (Array[String](), true),
      (Array("0"), true),
      (Array("0", "0"), true),
      (Array("0", "1"), true),
      (Array("0", "1", "2"), true),
      (Array("0", "2", "1"), false))
    forAll(testsString)(tableTest(gt[String]))
  }

  it should "work for random arrays" in {
    forAll("array") { as: Array[Int] =>
      def toSorted = { val sortedArray = as.clone(); Sorting.quickSort(sortedArray); sortedArray}
      val sortedArray = toSorted
      def isAlreadySorted = as.toSeq == sortedArray.toSeq

      assertResult(isAlreadySorted)(isSorted(as, gt[Int]))
      assertResult(true)(isSorted(sortedArray, gt[Int]))
    }
  }

}
