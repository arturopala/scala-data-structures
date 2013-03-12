import annotation.tailrec
import collection.mutable.ArrayBuffer

/** The (binary) heap data structure is an array object that can be viewed as a nearly complete binary tree */
trait Heap[@specialized(Int,Long,Double) N] {
self =>	

	val compare:(N,N) => Boolean
	
	private val A = ArrayBuffer[N]()
	private var count: Int = 0
	
	final def apply(i:Int): N = {
		if (i < 1 || i > count) throw new IndexOutOfBoundsException(s"index should be [1,$size) but is $i")
		A(i-1)
	}
	final def left(i:Int): Int = i*2
	final def right(i:Int): Int = i*2 + 1
	final def parent(i: Int): Int = i/2
	final def size: Int = count
	final def head: N = A.head 

	final def insert(elem: N): Unit = {
		A += elem
		count = count + 1
		bubbleUp(count)
	}
	
	final def extract: Option[N] = {
		if (count == 0) None
		else {
			val head = this(1)
			swap(1,count)
			count = count - 1
			bubbleDown(1)
			Some(head)
		}
	}

	private def update(i:Int,elem:N):Unit = {
		A(i-1) = elem
	}
	
	@tailrec
	private def bubbleDown(i:Int): Unit = {
		if (i > count) return
		val j: Int = check(i,left(i),right(i))
		if (j != i) { 
	        swap(j,i)
			bubbleDown(j)
		}
	}

	@tailrec
	private def bubbleUp(i:Int): Unit = {
		if (i <= 1) return
		val p = parent(i)
		val j: Int = check(p,left(p),right(p))
		if (j != p) {
			swap(j,p)
			bubbleUp(p)
		}
	}
	
	private def check(i:Int,l:Int,r:Int): Int = {
		if(l <= count && compare(this(l),this(i))) {
			if(r <= count && compare(this(r),this(l))) r else l
		} else {
			if(r <= count && compare(this(r),this(i))) r else i
		}
	}
	
	private def swap(i:Int, j:Int): Unit = {
		val e = this(i)
		this(i) = this(j)
		this(j) = e
	}

}

class MinHeap[@specialized(Int,Long,Double) N : Ordering] extends Heap[N] {
	val compare:(N,N) => Boolean = implicitly[Ordering[N]].lt
}

class MaxHeap[@specialized(Int,Long,Double) N : Ordering] extends Heap[N] {
	val compare:(N,N) => Boolean = implicitly[Ordering[N]].gt
}
