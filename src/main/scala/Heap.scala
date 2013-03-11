import annotation.tailrec
import collection.mutable.ArrayBuffer

class Heap[N : Ordering] {
	
	private val A = ArrayBuffer[N]()
	private val ordering = implicitly[Ordering[N]]
	import ordering._
	
	def apply(i:Int):N = A(i)
	def left(i:Int):Int = i*2
	def right(i:Int):Int = i*2 + 1
	def size:Int = A.size
	
	@tailrec
	final def heapify(i:Int): Unit = {
		val max: Int = maximum(i,left(i),right(i))
		if (max != i) { 
	        swap(max,i)
			heapify(max)
		}
	}
	
	private def maximum(i:Int,l:Int,r:Int): Int = {
		if(l < size && A(l) > A(i)) {
			if(r < size && A(r) > A(l)) r else l
		} else {
			if(r < size && A(r) > A(i)) r else i
		}
	}
	
	private def swap(i:Int, j:Int): Unit = {
		val e = A(i)
		A(i) = A(j)
		A(j) = e
	}

}
