package fpinscala.gettingstarted

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GettingStartedSuite  extends AnyFlatSpec with Matchers{

  "simple fib" should "return correct nth element" in {
    MyModule.fib(3) should equal(2)
    MyModule.fib(5) should equal(5)
    MyModule.fib(6) should equal(8)
  }

  "Polymorphic is sorted" should "work correctly for different kinds of types" in {
    PolymorphicFunctions.isSorted[Int](Array(1, 2, 3, 4), (x, y) => (x < y)) should be(true)
    PolymorphicFunctions.isSorted[Int](Array(1, 2, 3, 4), (x, y) => (x > y)) should be(false)
    PolymorphicFunctions.isSorted(Array("a", "aa", "abc", "abcd"), (x:String, y:String) => (x.length < y.length)) should be(true)
  }
}
