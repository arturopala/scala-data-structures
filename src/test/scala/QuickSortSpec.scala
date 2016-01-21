package me.arturopala

import collection.mutable.{ ArrayBuffer, Seq }
import org.scalatest.{ FunSpecLike, Matchers }
import scala.io.Source

class QuickSortSpec extends FunSpecLike with Matchers {

  describe("QuickSort") {

    it("swap elements in array") {
      val array = ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8)
      QuickSort.swap(array, 0, 1)
      array(0) shouldBe 1
      array(1) shouldBe 0
      array(2) shouldBe 2
    }

    it("should partition array") {
      for (i <- 0 to 8) {
        partitionAndCheck(ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8), i)
      }
      for (i <- 0 to 8) {
        partitionAndCheck(ArrayBuffer(4, 8, 6, 0, 1, 2, 3, 5, 7), i)
      }
      for (i <- 0 to 8) {
        partitionAndCheck(ArrayBuffer(12, 101, 33, 73, 49, 8, 61, 70, 52), i)
      }
    }

    it("should sort array with each strategy") {
      for (strategy <- Seq(QuickSort.first _, QuickSort.last _, QuickSort.median[Int] _)) {
        sortAndCheck(ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8), strategy)
        sortAndCheck(ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8).reverse, strategy)
        sortAndCheck(ArrayBuffer(4, 8, 6, 0, 1, 2, 3, 5, 7), strategy)
        sortAndCheck(ArrayBuffer(12, 101, 33, 73, 49, 8, 61, 70, 52), strategy)
        sortAndCheck(ArrayBuffer(12, 101, 33, 73, 49, 8, 61, 70, 52).reverse, strategy)
      }
    }

    it("should choose median") {
      QuickSort.median(ArrayBuffer(7, 12, 29), 0, 3) shouldBe 1
      QuickSort.median(ArrayBuffer(7, 29, 12), 0, 3) shouldBe 2
      QuickSort.median(ArrayBuffer(12, 7, 29), 0, 3) shouldBe 0
      QuickSort.median(ArrayBuffer(12, 29, 7), 0, 3) shouldBe 0
      QuickSort.median(ArrayBuffer(29, 7, 12), 0, 3) shouldBe 2
      QuickSort.median(ArrayBuffer(29, 12, 7), 0, 3) shouldBe 1
    }

    def partitionAndCheck[T](array: ArrayBuffer[T], pivotElementIndex: Int)(implicit ordering: Ordering[T]) = {
      val originalArray = array.clone
      val pivotIndex = QuickSort.partition(array, 0, array.length, QuickSort.first)
      val pivotElement = array(pivotIndex)
      for (i <- 0 until pivotIndex) {
        ordering.lteq(array(i), pivotElement) shouldBe true
      }
      for (i <- pivotIndex + 1 until array.length) {
        ordering.gteq(array(i), pivotElement) shouldBe true
      }
    }

    def sortAndCheck[T](array: Seq[T], pivotStrategy: (Seq[T], Int, Int) => Int)(implicit ordering: Ordering[T]) = {
      val originalArray = array.clone
      val count = QuickSort.sort(array, pivotStrategy)
      array.sliding(2).foreach(a => (ordering.lteq(a(0), a(1))) shouldBe true)
    }

    it("should sort dataset using `first` partitioning stategy") {
      val input: ArrayBuffer[Int] = ArrayBuffer() ++= (Source.fromFile("src/test/resources/quicksort.txt").getLines().map(_.toInt))
      sortAndCheck(input, QuickSort.first)
    }

    it("should sort dataset using `last` partitioning stategy") {
      val input: ArrayBuffer[Int] = ArrayBuffer() ++= (Source.fromFile("src/test/resources/quicksort.txt").getLines().map(_.toInt))
      sortAndCheck(input, QuickSort.last)
    }

    it("should sort dataset using `median` partitioning stategy") {
      val input: ArrayBuffer[Int] = ArrayBuffer() ++= (Source.fromFile("src/test/resources/quicksort.txt").getLines().map(_.toInt))
      sortAndCheck(input, QuickSort.median[Int])
    }

    it("should sort dataset using `random` partitioning stategy") {
      val input: ArrayBuffer[Int] = ArrayBuffer() ++= (Source.fromFile("src/test/resources/quicksort.txt").getLines().map(_.toInt))
      sortAndCheck(input, QuickSort.random)
    }
  }

}
