object QuickSort {

	def sort[T](array: Array[T])(pivotStrategy: (Array[T], Int, Int) => Int = chooseFirst _)(implicit ordering: Ordering[T]): Int = sort(array, 0, array.length)(pivotStrategy)

	def sort[T](array: Array[T], start: Int, end: Int)(pivotStrategy: (Array[T], Int, Int) => Int)(implicit ordering: Ordering[T]): Int = {
		var count = end - start - 1
		val pivotElementIndex = partition(array, start, end, pivotStrategy)
		if (pivotElementIndex > start) {
			val leftCount = sort(array, start, pivotElementIndex)(pivotStrategy)
			count = count + leftCount
		}
		if (pivotElementIndex + 1 < end) {
			val rightCount = sort(array, pivotElementIndex + 1, end)(pivotStrategy)
			count = count + rightCount
		}
		count
	}

	def partition[T](array: Array[T], start: Int, end: Int, pivotStrategy: (Array[T], Int, Int) => Int)(implicit ordering: Ordering[T]): Int = {

		assert(start < end)

		if (end - start == 1) return start
		val pivotElementIndex = pivotStrategy(array, start, end)

		assert(pivotElementIndex >= start)
		assert(pivotElementIndex < end)

		swap(array, pivotElementIndex, start)
		val pivot = array(start)
		var i = start;
		for (j <- start + 1 until end) {
			if (ordering.lt(array(j), pivot)) {
				i = i + 1
				swap(array, i, j)
			}
		}
		swap(array, start, i)
		i
	}

	def swap[T](array: Array[T], from: Int, to: Int): Unit = {
		if (from != to) {
			val elem: T = array(to)
			array(to) = array(from)
			array(from) = elem
		}
	}

	def chooseFirst(array: Array[_], start: Int, end: Int): Int = start

	def chooseLast(array: Array[_], start: Int, end: Int): Int = end - 1

	def chooseMedian[T](array: Array[T], start: Int, end: Int)(implicit ordering: Ordering[T]): Int = {
		val a = array(start)
		val b = array(end - 1)
		val middle = (start + end - 1) / 2
		val c = array(middle)
		import ordering._
		(a >= b, a >= c, b >= c) match {
			case (false, false, false) => end - 1 //a,b,c
			case (false, false, true) => middle //a,c,b
			case (true, false, false) => start //b,a,c
			case (true, true, false) => middle //b,c,a
			case (false, true, true) => start //c,a,b
			case (true, true, true) => end - 1 //c,b,a
			case _ => throw new IllegalStateException
		}


	}

}
