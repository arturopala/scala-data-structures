import collection.mutable.{Seq}

object QuickSort {

	/** Sorts mutable sequence in-place */
	def sort[T : Ordering](array: Seq[T]): Unit = sort(array, 0, array.length, chooseMedian[T] _)
	def sort[T : Ordering](array: Seq[T], pivotStrategy: (Seq[T], Int, Int) => Int): Unit = sort(array, 0, array.length, pivotStrategy)
	def sort[T : Ordering](array: Seq[T], start: Int, end: Int, pivotStrategy: (Seq[T], Int, Int) => Int): Unit = {
		val i = partition(array, start, end, pivotStrategy)
		if (i > start) sort(array, start, i, pivotStrategy)
		if (i + 1 < end) sort(array, i + 1, end, pivotStrategy)
	}

	def partition[T : Ordering](array: Seq[T], start: Int, end: Int, pivotStrategy: (Seq[T], Int, Int) => Int): Int = {
		val ordering = implicitly[Ordering[T]]
		if (end - start == 1) return start
		val p = pivotStrategy(array, start, end)
		swap(array, p, start)
		val pivot = array(start)
		var i = start
		for (j <- start + 1 until end) {
			if (ordering.lt(array(j), pivot)) {
				i = i + 1
				swap(array, i, j)
			}
		}
		swap(array, start, i)
		i
	}

	def swap[T](array: Seq[T], from: Int, to: Int): Unit = {
		if (from != to) {
			val elem: T = array(to)
			array(to) = array(from)
			array(from) = elem
		}
	}

	def chooseFirst(array: Seq[_], start: Int, end: Int): Int = start
	def chooseLast(array: Seq[_], start: Int, end: Int): Int = end - 1
	def chooseMedian[T : Ordering](array: Seq[T], start: Int, end: Int): Int = {
		val ordering = implicitly[Ordering[T]]
		import ordering._
		val a = array(start)
		val b = array(end - 1)
		val middle = (start + end - 1) / 2
		val c = array(middle)
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
