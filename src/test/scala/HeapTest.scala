package org.encalmo.algorithms

import org.scalatest.FunSpec
import scalax.file.Path

class HeapTest extends FunSpec {
	
    def fixture = new {
        val heap = new MinHeap[Int]
        heap insert 1221
        heap insert 1101
        heap insert 100
        heap insert 2
        heap insert 3
        heap insert 999
	    val maxHeap = new MaxHeap[Int]
	    maxHeap insert 1221
	    maxHeap insert 1101
	    maxHeap insert 100
	    maxHeap insert 2
	    maxHeap insert 3
	    maxHeap insert 999
    }

	describe("Heap") {
		it ("should calculate left, right and parent element index") {
            val f = fixture; import f._
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
		it ("should insert element while maintaining min-heap property") {
            val f = fixture; import f._
			assert(heap.head==2, s"heap head should be 2 but is ${heap.head}")
		}
		it ("should insert element while maintaining max-heap property") {
			val f = fixture; import f._
			assert(maxHeap.head==1221, s"heap head should be 2 but is ${maxHeap.head}")
		}
		it ("should extract head while maintaining heap property") {
            val f = fixture; import f._
			val elem = heap.extract.get
			assert(elem==2, s"extracted should be 2 but is ${elem}")
			assert(heap.head==3, s"heap head should be 3 but is ${heap.head}")
			val elem2 = heap.extract.get
			assert(elem2==3, s"extracted should be 3 but is ${elem2}")
			assert(heap.head==100, s"heap head should be 100 but is ${heap.head}")
		}
        it ("should remove element while maintaining heap property") {
            val f = fixture; import f._
            heap.remove(3)
            heap.remove(999)
            assert(heap.head==2, s"heap head should be 2 but is ${heap.head}")
            assert(heap.size==4, s"heap size should be 4 but is ${heap.size}")
            heap.extract
            assert(heap.head==100, s"heap head should be 100 but is ${heap.head}")
            heap.extract
            assert(heap.head==1101, s"heap head should be 1101 but is ${heap.head}")
        }
		it("should compute sum of moving medians") {
			val path = Path.fromString("src/main/resources/Median.txt")
			var sum = 0
			val minheap = new MinHeap[Int]()
			val maxheap = new MaxHeap[Int]()
			for (line <- path.lines()) {
				val num = line.trim.toInt
				if(maxheap.isEmpty){
					maxheap insert num
				} else {
					if(num <= maxheap.head) {
						maxheap insert num
					} else {
						minheap insert num
					}
				}
				if (maxheap.size > minheap.size + 1) {
					minheap insert (maxheap.extract)
				} else if (minheap.size > maxheap.size) {
					maxheap insert (minheap.extract)
				}
				val median = maxheap.head
				sum = sum + median
				if (sum >= 10000) sum = sum % 10000

			}
			assert(sum==1213)
		}
	}
	
}
