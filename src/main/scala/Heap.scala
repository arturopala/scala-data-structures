import annotation.tailrec
import collection.mutable.{Buffer, ArrayBuffer, Map}

trait Heap[@specialized(Int, Long, Double) N] {
    def head: N
    def size: Int
    def extract: Option[N]
    def insert(elem: N): Unit
    def remove(elem: N): Unit
    def refresh(elem: N): Unit
}

/** The (binary) heap data structure is an array-wrapping object that can be viewed as a nearly complete binary tree */
trait GenericHeap[@specialized(Int, Long, Double) N] extends Heap[N] {
    self =>

    protected val shouldBeHigher: (N, N) => Boolean
    protected val A: Buffer[N]
    private val map: Map[N,Int] = Map()

    protected var count: Int = 0

    @inline def apply(i: Int): N = A(i - 1)
    @inline protected def update(i: Int, elem: N): Unit = { 
        A(i - 1) = elem
        map(elem) = i
    }

    @inline final def left(i: Int): Int = i * 2
    @inline final def right(i: Int): Int = i * 2 + 1
    @inline final def parent(i: Int): Int = i / 2

    override final def size: Int = count
    override final def head: N = A.head
    
    override def insert(elem: N): Unit = {
        A += elem
        count = count + 1
        this(count) = elem
        bubbleUp(count)
    }

    override def extract: Option[N] = {
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

    @inline final def check(i: Int, l: Int, r: Int): Int = {
        if (l <= count && shouldBeHigher(this(l), this(i))) {
            if (r <= count && shouldBeHigher(this(r), this(l))) r else l
        } else {
            if (r <= count && shouldBeHigher(this(r), this(i))) r else i
        }
    }

    @inline final def swap(i: Int, j: Int): Unit = {
        val e = this(i)
        this(i) = this(j)
        this(j) = e
    }

    override def refresh(elem: N): Unit = {
        for(i <- map.get(elem)){
            if (shouldBeHigher(this(i),this(parent(i)))){
                bubbleUp(i)
            } else {
                bubbleDown(i)
            }
        }
    }

    override def remove(elem: N): Unit = {
        if (count == 0) return
        for(i <- map.get(elem)){
            swap(i, count)
            count = count - 1
            map.remove(elem)
            bubbleDown(i)
        }
    }

}

class MinHeap[@specialized(Int, Long, Double) N: Ordering](initialSize: Int = 16) extends GenericHeap[N] {
    override protected val A = new ArrayBuffer[N](initialSize)
    override protected val shouldBeHigher: (N, N) => Boolean = implicitly[Ordering[N]].lt
}

class MaxHeap[@specialized(Int, Long, Double) N: Ordering](initialSize: Int = 16) extends GenericHeap[N] {
    override protected val A = new ArrayBuffer[N](initialSize)
    override protected val shouldBeHigher: (N, N) => Boolean = implicitly[Ordering[N]].gt
}
