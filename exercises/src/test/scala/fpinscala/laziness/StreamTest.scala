package fpinscala.laziness

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class StreamTest extends AnyFlatSpec with Matchers with OptionValues {

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

  "take while" should "take only the expected items" in {
    Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 0).toList() shouldBe List()
    Stream(2, 3, 4, 5).takeWhile(_ % 2 == 0).toList() shouldBe List(2)
    Stream(2, 3, 4, 5).takeWhile(_ < 5).toList() shouldBe List(2, 3, 4)
    Stream(true, true, true, false).takeWhile(_ == true).toList() shouldBe List(true, true, true)
  }

  "for all" should "return correct answer" in {
    Stream(1, 2, 3, 4, 5).forAll(_ < 6) shouldBe true
    Stream(1, 2, 3, 4, 5).forAll(_ < 5) shouldBe false
    Stream(1, 2, 3, 4, 5).forAll(_ > 5) shouldBe false
    Stream[Int]().forAll(_ > 1) shouldBe true
  }

  "head option" should "return correct answer" in {
    Stream(1, 2).headOption.value shouldEqual 1
    Stream().headOption shouldEqual None

    Stream(1, 2).headOptionSol2.value shouldEqual 1
    Stream().headOptionSol2 shouldEqual None
  }

  "start with" should "return correct answer" in {
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2, 3, 4).startsWith(Stream(1)) shouldBe true
    Stream(1, 2, 3, 4).startsWith(Stream(2)) shouldBe false
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4)) shouldBe true
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 5)) shouldBe false
    Stream(1, 2, 3, 4).startsWith(Stream()) shouldBe false
    Stream().startsWith(Stream()) shouldBe true
  }

  "map stream" should "return mapped values" in {
    Stream(1, 2, 3, 4).map(_ * 2).toList() should equal(List(2, 4, 6, 8))
    Stream(1, 2, 3, 4).map(_ + 1).toList() should equal(List(2, 3, 4, 5))

    Stream(1, 2, 3, 4).mapUsingFoldRight(_ * 2).toList() should equal(List(2, 4, 6, 8))
    Stream(1, 2, 3, 4).mapUsingFoldRight(_ + 1).toList() should equal(List(2, 3, 4, 5))
  }

  "filter stream" should "return non filtered items only" in {
    Stream(1, 2, 3, 4).filter(_ > 3).toList() should equal(List(4))
  }

  "appending another stream" should "result in both streams appended" in {
    Stream(1, 2, 3).append(Stream(5,6)).toList() should equal(List(1, 2, 3 ,5, 6))
    Stream(1, 2, 3).append(Stream(5,6)).toList() should not equal(List(1, 2, 3 ,6, 5))
  }

  "flatmap of streams " should "return flattened streams" in {
    Stream(1, 2, 3).flatmap(x => Stream(x, x)).toList() should equal(List(1,1,2,2,3,3))
//    Stream[String]("hi", "there").flatmap(s => Stream(s.flatten)) should equal(List('h', 'i', 't', 'h','r', 'e'))
  }

  "constant" should "return const" in {
    Stream.constant(1).take(3).toList() should equal(Stream.ones.take(3).toList())
  }

  "from" should " return increasing series" in {
    Stream.from(1).take(3).toList() should equal(List(1, 2, 3))
  }
}
