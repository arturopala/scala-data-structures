package org.encalmo.algorithms

import annotation.tailrec
import collection.mutable
import collection.mutable.{Buffer, ArrayBuffer, Map}

trait Heap[@specialized(Int, Long, Double) N] extends Traversable[N] {
    def head: N
    def size: Int
	def contains (elem: N): Boolean
    def extract: Option[N]
    def insert(elem: N): Unit
	def insert(elements: Traversable[N]): Unit
    def remove(elem: N): Unit
	def remove(elements: Traversable[N]): Unit
    def refresh(elem: N): Unit
}

/** The (binary) heap data structure is an buffer-wrapping object that can be viewed as a nearly complete binary tree */
trait GenericHeap[@specialized(Int, Long, Double) N] extends Heap[N] {
    self =>

    protected val lower: (N, N) => Boolean
    protected val A: Buffer[N]
	
    private val map: Map[N,Int] = Map()
	private var count: Int = 0

    @inline private def apply(i: Int): N = A(i - 1)
    @inline private def update(i: Int, elem: N): Unit = { 
        A(i - 1) = elem
        map(elem) = i
    }

    @inline final def left(i: Int): Int = i * 2
    @inline final def right(i: Int): Int = i * 2 + 1
    @inline final def parent(i: Int): Int = i / 2

    override final def size: Int = count
    override final def head: N = A.head
	override final def foreach[U](f: N => U):Unit = for(i <- 0 until count) f(A(i))
	override final def contains (elem: N): Boolean = map.contains(elem)

	override final def insert(elements: Traversable[N]): Unit = {
		for (elem <- elements){
			insert(elem)
		}
	}
	
    override final def insert(elem: N): Unit = {
        A += elem
        count = count + 1
        this(count) = elem
        bubbleUp(count)
    }

    override final def extract: Option[N] = {
        if (count == 0) None
        else {
            val elem = head
            swap(1, count)
            count = count - 1
            map.remove(elem)
            bubbleDown(1)
            Some(elem)
        }
    }

    @tailrec
    protected final def bubbleDown(i: Int): Unit = {
        if (i > count) return
        val j: Int = check(i, left(i), right(i))
        if (j != i) {
            swap(j, i)
            bubbleDown(j)
        }
    }

    @tailrec
    protected final def bubbleUp(i: Int): Unit = {
        if (i <= 1) return
        val p = parent(i)
        val j: Int = check(p, left(p), right(p))
        if (j != p) {
            swap(j, p)
            bubbleUp(p)
        }
    }

    @inline private def check(i: Int, l: Int, r: Int): Int = {
        if (l <= count && lower(this(l), this(i))) {
            if (r <= count && lower(this(r), this(l))) r else l
        } else {
            if (r <= count && lower(this(r), this(i))) r else i
        }
    }

    @inline private def swap(i: Int, j: Int): Unit = {
        val e = this(i)
        this(i) = this(j)
        this(j) = e
    }

    override final def refresh(elem: N): Unit = {
        for(i <- map.get(elem)){
            if (lower(this(i),this(parent(i)))){
                bubbleUp(i)
            } else {
                bubbleDown(i)
            }
        }
    }

	override final def remove(elements: Traversable[N]): Unit = {
		for (elem <- elements){
			remove(elem)
		}
	}

    override final def remove(elem: N): Unit = {
        if (count == 0) return
        for(i <- map.get(elem)){
            swap(i, count)
            count = count - 1
            map.remove(elem)
            bubbleDown(i)
        }
    }
    
    override final def isEmpty: Boolean = count==0 

}

class MinHeap[@specialized(Int, Long, Double) N: Ordering](initialSize: Int = 16) extends GenericHeap[N] {
    override protected val A = new ArrayBuffer[N](initialSize)
    override protected val lower: (N, N) => Boolean = implicitly[Ordering[N]].lt
}

class MaxHeap[@specialized(Int, Long, Double) N: Ordering](initialSize: Int = 16) extends GenericHeap[N] {
    override protected val A = new ArrayBuffer[N](initialSize)
    override protected val lower: (N, N) => Boolean = implicitly[Ordering[N]].gt
}
