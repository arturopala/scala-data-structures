package me.arturopala

import org.scalatest.{ FunSpecLike, Matchers }
import scala.io.Source

class InversionCounterTest extends FunSpecLike with Matchers {

  describe("Heap") {

    it("should calculate left, right and parent element index") {

      val (s1, c1) = InversionCounter.sortAndCountInversions(Seq("1", "2", "3", "4", "5", "6"))
      c1 should be(0)
      val (s2, c2) = InversionCounter.sortAndCountInversions(Seq("2", "1"))
      c2 shouldBe 1
      val (s3, c3) = InversionCounter.sortAndCountInversions(Seq("3", "2", "1"))
      c3 shouldBe 3
      val (s4, c4) = InversionCounter.sortAndCountInversions(Seq("4", "3", "2", "1"))
      c4 shouldBe 6
      val (s5, c5) = InversionCounter.sortAndCountInversions(Seq("1", "2", "3", "4", "6", "5"))
      c5 shouldBe 1
      s5 shouldBe Seq("1", "2", "3", "4", "5", "6")
      val (s6, c6) = InversionCounter.sortAndCountInversions(Seq("4", "6", "7", "5"))
      c6 shouldBe 2
      val (s7, c7) = InversionCounter.sortAndCountInversions(Seq("1", "2", "3", "4", "6", "7", "5"))
      c7 shouldBe 2
      s7 shouldBe Seq("1", "2", "3", "4", "5", "6", "7")
      val (s8, c8) = InversionCounter.mergeAndCountSplitInversions(Seq("5"), Seq("4"))
      c8 shouldBe 1
      s8 shouldBe Seq("4", "5")
    }

    it("should sort and count inversions in given dataset") {
      val input: Seq[Integer] = Source.fromFile("src/test/resources/inversions.txt").getLines().map(Integer.valueOf(_)).toIndexedSeq
      val (sorted, count) = InversionCounter.sortAndCountInversions(input)
      count shouldBe 2407905288L
    }
  }

}