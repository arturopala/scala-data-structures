package org.encalmo.algorithms

import collection.mutable.{ArrayBuffer,Seq}
import org.junit.Assert._
import org.junit.Test
import scalax.file.Path

class QuickSortTest {

	@Test def testSwap = {
		val array = ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8)
		QuickSort.swap(array, 0, 1)
		assertEquals(1, array(0))
		assertEquals(0, array(1))
		assertEquals(2, array(2))
	}

	@Test def testPartition = {
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

	@Test def testSort = {
		for (strategy <- Seq(QuickSort.first _, QuickSort.last _, QuickSort.median[Int] _)) {
			sortAndCheck(ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8), strategy)
			sortAndCheck(ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8).reverse, strategy)
			sortAndCheck(ArrayBuffer(4, 8, 6, 0, 1, 2, 3, 5, 7), strategy)
			sortAndCheck(ArrayBuffer(12, 101, 33, 73, 49, 8, 61, 70, 52), strategy)
			sortAndCheck(ArrayBuffer(12, 101, 33, 73, 49, 8, 61, 70, 52).reverse, strategy)
		}
	}

	@Test def testChooseMedian = {
		assertEquals(1, QuickSort.median(ArrayBuffer(7, 12, 29), 0, 3))
		assertEquals(2, QuickSort.median(ArrayBuffer(7, 29, 12), 0, 3))
		assertEquals(0, QuickSort.median(ArrayBuffer(12, 7, 29), 0, 3))
		assertEquals(0, QuickSort.median(ArrayBuffer(12, 29, 7), 0, 3))
		assertEquals(2, QuickSort.median(ArrayBuffer(29, 7, 12), 0, 3))
		assertEquals(1, QuickSort.median(ArrayBuffer(29, 12, 7), 0, 3))
	}

	private def partitionAndCheck[T](array: ArrayBuffer[T], pivotElementIndex: Int)(implicit ordering: Ordering[T]) = {
		val originalArray = array.clone
		val pivotIndex = QuickSort.partition(array, 0, array.length, QuickSort.first)
		val pivotElement = array(pivotIndex)
		for (i <- 0 until pivotIndex) {
			assertTrue(ordering.lteq(array(i), pivotElement))
		}
		for (i <- pivotIndex + 1 until array.length) {
			assertTrue(ordering.gteq(array(i), pivotElement))
		}
	}

	private def sortAndCheck[T](array: Seq[T], pivotStrategy: (Seq[T], Int, Int) => Int)(implicit ordering: Ordering[T]) = {
		val originalArray = array.clone
		val count = QuickSort.sort(array, pivotStrategy)
		array.sliding(2).foreach(a => assertTrue(ordering.lteq(a(0), a(1))))
	}
	
	@Test def doHomework1 = {
		val input: ArrayBuffer[Int] = ArrayBuffer() ++= (Path.fromString("src/main/resources/quicksort.txt").lines().map(_.toInt))
		sortAndCheck(input, QuickSort.first)
	}

	@Test def doHomework2 = {
		val input: ArrayBuffer[Int] = ArrayBuffer() ++= (Path.fromString("src/main/resources/quicksort.txt").lines().map(_.toInt))
		sortAndCheck(input, QuickSort.last)
	}

	@Test def doHomework3 = {
		val input: ArrayBuffer[Int] = ArrayBuffer() ++= (Path.fromString("src/main/resources/quicksort.txt").lines().map(_.toInt))
		sortAndCheck(input, QuickSort.median[Int])
	}

	@Test def doHomework4 = {
		val input: ArrayBuffer[Int] = ArrayBuffer() ++= (Path.fromString("src/main/resources/quicksort.txt").lines().map(_.toInt))
		sortAndCheck(input, QuickSort.random)
	}

}
