package fpinscala.laziness

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class StreamTest extends AnyFlatSpec with Matchers {

  "toList" should "list Stream" in {
    Stream(1, 2, 3).toList() shouldBe List(1,2,3)
  }

  "Empty stream toList" should "return empty list" in {
    Stream().toList() shouldBe List()
    Stream().toList() shouldBe List.empty
  }

  "take stream" should "take the correct amount of items" in {
    Stream[Int](1, 2,3).take(2).toList() shouldBe List(1, 2)
  }

  "drop stream" should "drop the correct amount of items" in {
    Stream(1,2,3).drop(0).toList() shouldBe List(1, 2, 3)
    Stream(1,2,3).drop(1).toList() shouldBe List(2, 3)
    Stream(1,2,3).drop(2).toList() shouldBe List(3)
    Stream(1,2,3).drop(3).toList() shouldBe List()
    Stream(1,2,3).drop(4).toList() shouldBe List()

    Stream(1,2,3).dropSol2(0).toList() shouldBe List(1, 2, 3)
    Stream(1,2,3).dropSol2(1).toList() shouldBe List(2, 3)
    Stream(1,2,3).dropSol2(2).toList() shouldBe List(3)
    Stream(1,2,3).dropSol2(3).toList() shouldBe List()
    Stream(1,2,3).dropSol2(4).toList() shouldBe List()
  }
}
