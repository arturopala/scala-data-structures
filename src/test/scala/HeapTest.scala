import org.scalatest.FunSpec

class HeapTest extends FunSpec {
	
	val heap = new MinHeap[Int]
	heap insert 1221
	heap insert 1101
	heap insert 100
	heap insert 2
	heap insert 3
	heap insert 999

	describe("Heap") {
		it ("should calculate left, right and parent element index") {
			assert(heap.left(1)==2, s"left of 1 should be 2")
			assert(heap.right(1)==3, s"right of 1 should be 3")
			assert(heap.left(2)==4, s"left of 2 should be 4")
			assert(heap.right(2)==5, s"right of 2 should be 5")
			assert(heap.left(3)==6, s"left of 3 should be 6")
			assert(heap.right(3)==7, s"right of 3 should be 7")
			assert(heap.parent(2)==1, s"parent of 2 should be 1")
			assert(heap.parent(3)==1, s"parent of 3 should be 1")
			assert(heap.parent(6)==3, s"parent of 6 should be 3")
			assert(heap.parent(7)==3, s"parent of 7 should be 3")
		}
		it ("should insert element while maintaining heap property") {
			assert(heap.head==2, s"heap head should be 2 but is ${heap.head}")
		}
		it ("should extract head while maintaining heap property") {
			val elem = heap.extract.get
			assert(elem==2, s"extracted should be 2 but is ${elem}")
			assert(heap.head==3, s"heap head should be 3 but is ${heap.head}")
			val elem2 = heap.extract.get
			assert(elem2==3, s"extracted should be 3 but is ${elem2}")
			assert(heap.head==100, s"heap head should be 100 but is ${heap.head}")
		}
	}
	
}
