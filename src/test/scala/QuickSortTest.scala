import org.junit.Assert._
import org.junit.Test
import scalax.file.Path

class QuickSortTest {

	@Test def testSwap = {
		val array = Array(0, 1, 2, 3, 4, 5, 6, 7, 8)
		QuickSort.swap(array, 0, 1)
		assertEquals(1, array(0))
		assertEquals(0, array(1))
		assertEquals(2, array(2))
	}

	@Test def testPartition = {
		for (i <- 0 to 8) {
			partitionAndCheck(Array(0, 1, 2, 3, 4, 5, 6, 7, 8), i)
		}
		for (i <- 0 to 8) {
			partitionAndCheck(Array(4, 8, 6, 0, 1, 2, 3, 5, 7), i)
		}
		for (i <- 0 to 8) {
			partitionAndCheck(Array(12, 101, 33, 73, 49, 8, 61, 70, 52), i)
		}
	}

	@Test def testSort = {
		for (strategy <- Seq(QuickSort.chooseFirst _, QuickSort.chooseLast _, QuickSort.chooseMedian[Int] _)) {
			sortAndCheck(Array(0, 1, 2, 3, 4, 5, 6, 7, 8), strategy)
			sortAndCheck(Array(0, 1, 2, 3, 4, 5, 6, 7, 8).reverse, strategy)
			sortAndCheck(Array(4, 8, 6, 0, 1, 2, 3, 5, 7), strategy)
			sortAndCheck(Array(12, 101, 33, 73, 49, 8, 61, 70, 52), strategy)
			sortAndCheck(Array(12, 101, 33, 73, 49, 8, 61, 70, 52).reverse, strategy)
		}
	}

	@Test def testChooseMedian = {
		assertEquals(1, QuickSort.chooseMedian(Array(7, 12, 29), 0, 3))
		assertEquals(2, QuickSort.chooseMedian(Array(7, 29, 12), 0, 3))
		assertEquals(0, QuickSort.chooseMedian(Array(12, 7, 29), 0, 3))
		assertEquals(0, QuickSort.chooseMedian(Array(12, 29, 7), 0, 3))
		assertEquals(2, QuickSort.chooseMedian(Array(29, 7, 12), 0, 3))
		assertEquals(1, QuickSort.chooseMedian(Array(29, 12, 7), 0, 3))
	}

	private def partitionAndCheck[T](array: Array[T], pivotElementIndex: Int)(implicit ordering: Ordering[T]) = {
		val originalArray = array.clone();
		val pivotIndex = QuickSort.partition(array, 0, array.length, QuickSort.chooseFirst)
		val pivotElement = array(pivotIndex)
		Console.println(pivotIndex + ":" + pivotElement + " " + originalArray.mkString("(", ",", ")") + " -> " + array.mkString("(", ",", ")"))
		for (i <- 0 until pivotIndex) {
			assertTrue(ordering.lteq(array(i), pivotElement))
		}
		for (i <- pivotIndex + 1 until array.length) {
			assertTrue(ordering.gteq(array(i), pivotElement))
		}
	}

	private def sortAndCheck[T](array: Array[T], pivotStrategy: (Array[T], Int, Int) => Int)(implicit ordering: Ordering[T]) = {
		val originalArray = array.clone();
		val count = QuickSort.sort(array)(pivotStrategy)
		Console.println("#" + count + ": " + originalArray.mkString("(", ",", ")") + " -> " + array.mkString("(", ",", ")"))
		array.sliding(2).foreach(a => assertTrue(ordering.lteq(a(0), a(1))))
	}

	@Test def doHomework1 = {
		val input: Array[Int] = Path.fromString("src/main/resources/quicksort.txt").lines().map(Integer.parseInt(_)).toArray
		val count = QuickSort.sort(input)(QuickSort.chooseFirst)
		Console.println("#first: " + count)
	}

	@Test def doHomework2 = {
		val input: Array[Int] = Path.fromString("src/main/resources/quicksort.txt").lines().map(Integer.parseInt(_)).toArray
		val count = QuickSort.sort(input)(QuickSort.chooseLast)
		Console.println("#last: " + count)
	}

	@Test def doHomework3 = {
		val input: Array[Int] = Path.fromString("src/main/resources/quicksort.txt").lines().map(Integer.parseInt(_)).toArray
		val count = QuickSort.sort(input)(QuickSort.chooseMedian[Int])
		Console.println("#median: " + count)
	}

}
