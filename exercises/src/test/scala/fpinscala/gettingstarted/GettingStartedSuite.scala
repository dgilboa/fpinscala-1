package fpinscala.gettingstarted

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GettingStartedSuite  extends AnyFlatSpec with Matchers{

  "simple fib" should "return correct nth element" in {
    MyModule.fib(3) should equal(2)
    MyModule.fib(5) should equal(5)
    MyModule.fib(6) should equal(8)
  }
}
