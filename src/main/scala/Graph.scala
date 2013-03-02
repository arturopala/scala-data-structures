import scala.specialized
import scalax.file.Path

trait Graph[@specialized(Int) N] extends Traversable[(N,N)] {
    def nodes: Iterable[N]
    def adjacent: N => Traversable[N]
    def edges: Traversable[(N,N)]
    def reverse: Graph[N]
	def nodesCount:Int
	def edgesCount:Long
}

trait Weighted[@specialized(Int) N, @specialized(Double) V] {
    def weight: (N,N) => V
}

trait GenericGraph[@specialized(Int) N] extends Graph[N] {
	self =>
    override def edges: Traversable[(N,N)] = this
    override def foreach[U](f: ((N, N)) => U):Unit = for(from <- nodes; to <- adjacent(from)) f((from,to))
    override lazy val reverse:Graph[N] = new GenericReverseGraph[N](self)
    override def nodesCount: Int = nodes.size
    override def edgesCount: Long = nodes.foldLeft(0L){case (sum,node) => sum + adjacent(node).size}
}

class GenericReverseGraph[@specialized(Int) N](origin: Graph[N]) extends Graph[N] {
    override def nodes: Iterable[N] = origin.nodes
    override val adjacent: N => Traversable[N] = node => new Traversable[N]{
        def foreach[U](f: (N) => U) {
            for (n <- origin.nodes if (origin.adjacent(n) match {
                case set:Set[N] => set contains node
                case col => col exists (_ == node)
            })) f(n)
        }
    }
    override def edges: Traversable[(N,N)] =  this
    override def foreach[U](f: ((N, N)) => U):Unit = for(from <- origin.nodes; to <- origin.adjacent(from)) f((to,from))
    override val reverse = origin
    override def nodesCount: Int = origin.nodesCount
    override def edgesCount: Long = origin.edgesCount
}

object Graph {

    def apply[@specialized(Int) N](mappings:(N,Iterable[N])*): Graph[N] = new GenericGraph[N] {
        val map = mappings.toMap
        val nodes: Iterable[N] = map.keys
        val adjacent: N => Iterable[N] = map
    }

	def readFromEdgeListFile(path:Path):Graph[Int] = {
		val nodeMap = scala.collection.mutable.Map[Int,scala.collection.mutable.ArrayBuffer[Int]]()
		for (line <-path.lines() if !line.trim.isEmpty) {
			val i = line.indexOf(' ')
			val tail = line.substring(0,i).toInt
			val head = line.substring(i+1).trim.toInt
			val adjacent = nodeMap.getOrElseUpdate(tail,{scala.collection.mutable.ArrayBuffer[Int]()})
			adjacent += head
		}
		new GenericGraph[Int] {
			val nodes: Iterable[Int] = nodeMap.keys
			val adjacent: (Int) => Traversable[Int] = nodeMap
		}
	}
}

