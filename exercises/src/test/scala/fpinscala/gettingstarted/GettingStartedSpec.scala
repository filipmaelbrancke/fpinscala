package fpinscala.gettingstarted

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

import org.scalacheck.Gen

import MyModule.fib

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

}
