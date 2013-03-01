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

trait GraphBase[@specialized(Int) N] extends Graph[N] {
	self =>

    override def edges: Traversable[(N,N)] = self
    override def foreach[U](f: ((N, N)) => U) {
        for(from <- nodes; to <- adjacent(from)) f((from,to))
    }
    override def reverse:Graph[N] = new GraphBase[N] {
        override def nodes: Iterable[N] = self.nodes
        override val adjacent: N => Traversable[N] = node => new Traversable[N]{
            def foreach[U](f: (N) => U) {
               for (n <- self.nodes if (self.adjacent(n) match {
		            case set:Set[N] => set contains node
		            case col => col exists (_ == node)
	            })) f(n)
            }
	        def contains(col:Traversable[N],node:N):Boolean = {
		        col match {
			        case set:Set[N] => set.contains(node)
			        case _ => col exists (n => n==node)
		        }
	        }
        }
        override val edges: Traversable[(N,N)] =  new Traversable[(N,N)] {
            def foreach[U](f: ((N,N)) => U) {
                for(from <- self.nodes; to <- self.adjacent(from)) f((to,from))
            }
        }
        override val reverse = self
    }

	def nodesCount: Int = nodes.size
	def edgesCount: Long = nodes.foldLeft(0L){case (sum,node) => sum + adjacent(node).size}
}

object Graph {
    def apply[@specialized(Int) N](mappings:(N,Iterable[N])*): Graph[N] = new GraphBase[N] {
        val map = mappings.toMap
        def nodes: Iterable[N] = map.keys
        def adjacent: N => Iterable[N] = map
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
		new GraphBase[Int] {
			val nodes: Iterable[Int] = nodeMap.keys
			def adjacent: (Int) => Traversable[Int] = nodeMap
		}
	}
}

