import collection.mutable
import com.sun.org.apache.xalan.internal.lib.NodeInfo
import scala.specialized
import scalax.file.Path
import scala.collection.mutable.{Stack,ArrayBuffer}

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
    override def reverse:Graph[N] = new GenericReverseGraph[N](self)
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

class MutableMapGraph[@specialized(Int) N] extends GenericGraph[N] {
    protected val map = scala.collection.mutable.Map[N,ArrayBuffer[N]]()
    override def nodes:Iterable[N] =  map.keys
    override val adjacent: N => ArrayBuffer[N] = map.withDefaultValue(ArrayBuffer.empty[N])

    def add(edge: (N,N)):MutableMapGraph[N] = {
        map.getOrElseUpdate(edge._1,{ArrayBuffer[N]()}) += (edge._2); this
    }
    def add(edges:Traversable[(N,N)]):MutableMapGraph[N] = {
        for (edge <- edges) add(edge); this
    }
    def addReverse(edge: (N,N)):MutableMapGraph[N] = {
        map.getOrElseUpdate(edge._2,{ArrayBuffer[N]()}) += (edge._1); this
    }
    def addReverse(edges:Traversable[(N,N)]):MutableMapGraph[N] = {
        for (edge <- edges) addReverse(edge); this
    }
}

object Graph {

    def apply[@specialized(Int) N](mappings:(N,Iterable[N])*): Graph[N] = new GenericGraph[N] {
        private val map = mappings.toMap
        val nodes: Iterable[N] = map.keys
        val adjacent: N => Traversable[N] = map.withDefaultValue(Traversable.empty[N])
    }

    def apply[@specialized(Int) N](edges:Traversable[(N,N)]): Graph[N] = new MutableMapGraph[N]().add(edges)

	def readFromEdgeListFile(path:Path, reversed:Boolean = false):Graph[Int] = {
        val edges = path.lines().view map (line => {
            val i = line.indexOf(' ')
            val tail = line.substring(0,i).toInt
            val head = line.substring(i+1).trim.toInt
            if (reversed) (head,tail) else (tail,head)
        })
        Graph(edges)
	}

    def reverse[N](graph:Graph[N]):Graph[N] =  new MutableMapGraph[N](){
        override lazy val reverse = graph
    }.addReverse(graph)

    trait DfsLoopObserver[N] {
        def beforeOuter(node:N) {}
        def beforeInner(node:N) {}
        def afterInner(node:N) {}
    }

    class SccNodeInfo[N] {
        var leader:Option[N] = None
        var time:Int = 0
    }

    def scc[@specialized(Int) N](graph:Graph[N]): Seq[(N,Iterable[N])] = {
        //first pass
        val reversed: Graph[N] = Graph.reverse(graph)
        val attributes = mutable.Map[N,SccNodeInfo[N]]()
        def attrOf(node:N):SccNodeInfo[N] = attributes.getOrElseUpdate(node,{new SccNodeInfo[N]})
        var t:Int = 0
        var s: Option[N] = None
        val observer1 = new DfsLoopObserver[N] {
            override def afterInner(node:N) {
                t = t + 1
                attrOf(node).time = t
            }
        }
        dfs(reversed,observer1)(reversed.nodes)
        // second pass
        val ordered = (attributes map {case (node,attr) => (node,attr.time)}).toSeq sortBy {case (_,time) => -time} map {case (node,_) => node}
        val observer2 = new DfsLoopObserver[N] {
            override def beforeOuter(node:N) {
                s = Some(node)
            }
            override def beforeInner(node:N) {
                attrOf(node).leader = s
            }
        }
        dfs(graph,observer2)(ordered)
        //result
        val result = (attributes groupBy {case (_,attr) => attr.leader.get}).toSeq sortBy {case (_,map) => -map.size} map {case (n,map) => (n,map.keys)}
        result
    }

    def dfs[@specialized(Int) N](graph:Graph[N], observer: DfsLoopObserver[N])(nodes:Iterable[N] = graph.nodes):Unit = {
        val explored = mutable.HashSet[N]()
        for (node <- nodes){
            if (!(explored contains node)){
                observer beforeOuter (node)
                dfs(graph,node,observer,explored)
            }
        }
    }

    def dfs[@specialized(Int) N](graph:Graph[N],node:N, observer: DfsLoopObserver[N], explored:mutable.HashSet[N]):Unit = {
        if (!(explored contains node)){
            explored add node
            observer beforeInner node
            val next = graph.adjacent(node) filterNot explored
            if (!next.isEmpty) next foreach {n =>
                dfs(graph,n,observer,explored)
            }
            observer afterInner node
        }
    }
}

