import collection.mutable
import com.sun.org.apache.xalan.internal.lib.NodeInfo
import scala.specialized
import scalax.file.Path
import scala.collection.mutable.{Stack,ArrayBuffer}

trait Graph[@specialized(Int) N] {
    def nodes: Iterable[N]
    def adjacent: N => Traversable[N]
    def edges: Traversable[(N,N)]
    def reverse: Graph[N]
	def nodesCount:Int
	def edgesCount:Long
}

trait Unidirected[@specialized(Int) N] extends Graph[N] {
	override val reverse: Graph[N] = this
}

trait Weighted[@specialized(Int) N, @specialized(Int,Double) V] {
    def weight: (N,N) => V
}

trait GenericGraph[@specialized(Int) N] extends Graph[N] {
	self =>
    override def edges: Traversable[(N,N)] = new Traversable[(N, N)] {
	    override def foreach[U](f: ((N, N)) => U):Unit = for(from <- nodes; to <- adjacent(from)) f((from,to))
    }
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
    override def edges: Traversable[(N,N)] =  new Traversable[(N, N)] {
	    def foreach[U](f: ((N, N)) => U) {
		    for(from <- origin.nodes; to <- origin.adjacent(from)) f((to,from))
	    }
    }
    override val reverse = origin
    override def nodesCount: Int = origin.nodesCount
    override def edgesCount: Long = origin.edgesCount
}

class MapAsGraph[@specialized(Int) N](val nodeMap: scala.collection.Map[N,Traversable[N]]) extends GenericGraph[N] {
	override val nodes:Iterable[N] =  nodeMap.keys
	override val adjacent: N => Traversable[N] = nodeMap
	override lazy val reverse = Graph.copyReversed[N](this)
}

class MutableGraph[@specialized(Int) N] extends GenericGraph[N] {
    protected val nodeMap = scala.collection.mutable.Map[N,ArrayBuffer[N]]()
    override def nodes:Iterable[N] =  nodeMap.keys
    override val adjacent: N => ArrayBuffer[N] = nodeMap
	override def reverse = Graph.copyReversed[N](this)
	override def nodesCount: Int = nodeMap.size
	
	def add(node:N, adjacent:ArrayBuffer[N] = ArrayBuffer.empty):MutableGraph[N] = {
		nodeMap.update(node,adjacent)
		adjacent foreach (nodeMap.getOrElseUpdate(_,{new ArrayBuffer[N]()}))
		this
	} 
    def add(edge: (N,N)):MutableGraph[N] = {
        nodeMap.getOrElseUpdate(edge._1,{new ArrayBuffer[N]()}) += (edge._2)
	    nodeMap.getOrElseUpdate(edge._2,{new ArrayBuffer[N]()})
	    this
    }
    def add(edges:Traversable[(N,N)]):MutableGraph[N] = {
        for (edge <- edges) add(edge); this
    }
    def addReverse(edge: (N,N)):MutableGraph[N] = add(edge.swap)
    def addReverse(edges:Traversable[(N,N)]):MutableGraph[N] = {
        for (edge <- edges) add(edge.swap); this
    }
}

object Graph {
	
	class GenericGraphImpl[@specialized(Int) N](val nodes:Iterable[N], val adjacent: N => Traversable[N]) extends GenericGraph[N]
	class WeightedGraphImpl[@specialized(Int) N, @specialized(Double,Int) V:Numeric](val nodes:Iterable[N], val adjacent: N => Traversable[N], val weight: (N,N) => V) extends GenericGraph[N] with Weighted[N,V]

	def apply[@specialized(Int) N](): Graph[N] = new MutableGraph[N]()
	def apply[@specialized(Int) N](map: Map[N,Traversable[N]]): Graph[N] = new MapAsGraph(map)
    def apply[@specialized(Int) N](mappings:(N,Traversable[N])*): Graph[N] = new MapAsGraph(mappings.toMap)
	def apply[@specialized(Int) N](nodes:Iterable[N], adjacent: N => Traversable[N]): Graph[N] = new GenericGraphImpl[N](nodes,adjacent)
    def apply[@specialized(Int) N](edges:Traversable[(N,N)]): Graph[N] = new MutableGraph[N]().add(edges)
	def apply[@specialized(Int) N, @specialized(Double,Int) V:Numeric](mappings:(N,Iterable[(N,V)])*): Graph[N] with Weighted[N,V] = {
		val nodeWeightMap = mappings.toMap map {case (k,v) => (k,v.toMap)}
		new WeightedGraphImpl[N,V](nodeWeightMap.keys, nodeWeightMap.mapValues{case m => m.keys}, (t:N,h:N) => nodeWeightMap(t)(h))
	}

	def readFromEdgeListFile(path:Path, reversed:Boolean = false):Graph[Int] = {
        val edges = path.lines().view map (line => {
            val i = line.indexOf(' ')
            val tail = line.substring(0,i).toInt
            val head = line.substring(i+1).trim.toInt
            if (reversed) (head,tail) else (tail,head)
        })
        Graph(edges)
	}

	def readFromAdjacentListFile(path:Path): Graph[Int] = {
		def parseNodeAdjacentList(line:String):(Int,Seq[Int]) = {
			val tokens = line.split('\t')
			if(tokens.length==0) return null
			val label:Int = Integer.parseInt(tokens(0))
			val adjacent:Seq[Int] = tokens.drop(1) map (_.toInt)
			(label,adjacent)
		}
		val nodeMap = scala.collection.mutable.Map[Int,Traversable[Int]]()
		for (line <-path.lines() if !line.trim.isEmpty) {
			val (node, adjacent) = parseNodeAdjacentList(line)
			nodeMap(node) = adjacent
		}
		new GenericGraphImpl[Int](nodeMap.keys, nodeMap)
	}

	def readFromAdjacentWeightListFile(path:Path): Graph[Int] with Weighted[Int,Int] = {
		def parseNodeWeightAdjacentList(line:String):(Int,Map[Int,Int]) = {
			val tokens = line.split('\t')
			if(tokens.length==0) return null
			val label:Int = Integer.parseInt(tokens(0))
			val adjacent:Map[Int,Int] = (tokens.drop(1) map parseNodeWeight _).toMap
			(label,adjacent)
		}
		def parseNodeWeight(token:String): (Int,Int) = {
			val nw = token.split(',') map (_.toInt); (nw(0),nw(1))
		}
		val nodeWeightMap = scala.collection.mutable.Map[Int,Map[Int,Int]]()
		for (line <-path.lines() if !line.trim.isEmpty) {
			val (node, list) = parseNodeWeightAdjacentList(line)
			nodeWeightMap(node) = list
		}
		new WeightedGraphImpl[Int,Int](nodeWeightMap.keys, nodeWeightMap.mapValues{case m => m.keys}, (t:Int,h:Int) => nodeWeightMap(t)(h))
	}

    def copyReversed[@specialized(Int) N](graph:Graph[N]):Graph[N] =  new MutableGraph[N](){
        override lazy val reverse = graph
    }.addReverse(graph.edges)

    trait Observer[@specialized(Int) N] {
        def start(node:N) {}
        def before(node:N) {}
	    def edge(edge:(N,N)) {}
        def after(node:N) {}
    }

	def dfs[@specialized(Int) N](graph:Graph[N], observer: Observer[N]):Unit = dfs(graph, observer, graph.nodes)
	def dfs[@specialized(Int) N](graph:Graph[N], observer: Observer[N], nodes:Iterable[N]):Unit = {
		val explored = new mutable.HashSet[N]()
		for (node <- nodes){
			if (!(explored contains node)){
				observer start node
				dfs(graph,node,observer,explored)
			}
		}
	}

	def dfs[@specialized(Int) N](graph:Graph[N],node:N, observer: Observer[N], explored:mutable.HashSet[N] = mutable.HashSet[N]()):Unit = {
		if (!(explored contains node)){
			explored add node
			observer before node
			val next = graph.adjacent(node) filterNot explored
			if (!next.isEmpty) next foreach {n =>
				observer edge ((node,n))
				dfs(graph,n,observer,explored)
			}
			observer after node
		}
	}

	def findCycles[@specialized(Int) N](graph:Graph[N]): Vector[N] = {
		var cycles: Vector[N] = Vector.empty[N]
		val marks = new mutable.HashMap[N,Char]().withDefaultValue('0')
		for (node <- graph.nodes if (marks(node) == '0')) {
			cycles = cycles ++ findCycles(graph,node,marks)
		}
		cycles
	}

	def findCycles[@specialized(Int) N](graph:Graph[N], node: N, marks: mutable.Map[N,Char] = new mutable.HashMap[N,Char]().withDefaultValue('0')): Vector[N] = {
		var cycles: Vector[N] = Vector.empty[N]
		if (marks(node) == 'x') cycles = cycles :+ node
		else if (marks(node) == '0'){
			marks(node) = 'x'
			graph.adjacent(node) foreach {next =>
				cycles = cycles ++ findCycles(graph,next,marks)
			}
			marks(node) = '1'
		}
		cycles
	}

	private object CycleFoundException extends Exception
	
	def hasCycles[@specialized(Int) N](graph:Graph[N]): Boolean = {
		val marks = new mutable.HashMap[N,Char]().withDefaultValue('0')
		def checkCycles(node: N): Unit = {
			if (marks(node) == 'x') throw CycleFoundException
			else if (marks(node) == '0'){
				marks(node) = 'x'
				graph.adjacent(node) foreach checkCycles
				marks(node) = '1'
			}
		}
		try {
			for (node <- graph.nodes if (marks(node) == '0')) checkCycles(node)
			false
		}
		catch {
			case CycleFoundException => true
		}
	}

    /** Kosaraju's two dfs pass algorithm finds strongly connected components */
    def findStronglyConnectedComponents[@specialized(Int) N](graph:Graph[N]): Iterable[Iterable[N]] = {
	    class SccNodeInfo {
		    var leader:Option[N] = None
		    var time:Int = 0
	    }
	    val attributes = new mutable.HashMap[N,SccNodeInfo]()
	    def attrOf(node:N):SccNodeInfo = attributes.getOrElseUpdate(node,{new SccNodeInfo})
        // first pass
        val reversed: Graph[N] = graph.reverse
        var t:Int = 0
        var s: Option[N] = None
        val observer1 = new Observer[N] {
            override def after(node:N) {
                t = t + 1
                attrOf(node).time = t
            }
        }
        dfs(reversed,observer1,reversed.nodes)
        // second pass
        val ordered = (attributes map {case (node,attr) => (node,attr.time)}).toSeq sortBy {case (_,time) => -time} map {case (node,_) => node}
        val observer2 = new Observer[N] {
            override def start(node:N) {
                s = Some(node)
            }
            override def before(node:N) {
                attrOf(node).leader = s
            }
        }
        dfs(graph,observer2,ordered)
        // compute result
        val result = (attributes groupBy {case (_,attr) => attr.leader.get}).toSeq sortBy {case (_,map) => -map.size} map {case (n,map) => map.keys}
        result
    }
	
	/** Dijkstra algorithm finds shortest path in acyclic directed graph */ 
	def findShortestPath[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N] with Weighted[N,V],from: N, to: N): (V,Iterable[(N,N)]) = {
		val num: Numeric[V] = implicitly[Numeric[V]]
		if(from==to || graph.adjacent(from).isEmpty) return (num.zero,Nil)
		val nodesCount = graph.nodesCount
		val explored = new mutable.HashSet[N]()
		val distance = new mutable.HashMap[N,V]()
		val breadcrumbs = new ArrayBuffer[(N,N)]()
		val outgoingEdges = new mutable.HashSet[(N,N)]()
		var head = from
		explored add from
		distance(from) = num.zero
		var nextEdges = graph.adjacent(from) filterNot explored map (node => (from,node))
		outgoingEdges ++= nextEdges
		do {
			val (t,h) = outgoingEdges minBy {case (t,h) => num.plus(distance(t),graph.weight(t,h))}
			explored add h
			distance(h) = num.plus(distance(t),graph.weight(t,h))
			breadcrumbs += ((t,h))
			outgoingEdges remove (t,h)
			outgoingEdges --= (outgoingEdges filter {case (_,node) => node == h})
			nextEdges = graph.adjacent(h) filterNot explored map (node => (h,node))
			outgoingEdges ++= nextEdges
			head = h
		} while (head!=to && !outgoingEdges.isEmpty && explored.size != nodesCount)
		// compute resulting path
		var path: List[(N,N)] = Nil
		if(head==to){
			val backgraph = Graph(breadcrumbs map (_.swap))
			var next = to
			do {
				val node = next
				next = backgraph.adjacent(node).minBy(n => distance(n))
				val segment = (next,node)
				path = segment :: path
			} while(next!=from)
		}
		(distance(to), path)
	}

	/** Dijkstra algorithm finds all shortest paths starting at given node in acyclic directed graph */
	def findShortestPaths[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N] with Weighted[N,V],from: N): scala.collection.Map[N,V] = {
		val num: Numeric[V] = implicitly[Numeric[V]]
		if(graph.adjacent(from).isEmpty) return Map.empty
		val nodesCount = graph.nodesCount
		val explored = new mutable.HashSet[N]()
		val distance = new mutable.HashMap[N,V]()
		val outgoingEdges = new mutable.HashSet[(N,N)]()
		explored add from
		distance(from) = num.zero
		var nextEdges = graph.adjacent(from) filterNot explored map (node => (from,node))
		outgoingEdges ++= nextEdges
		do {
			val (t,h) = outgoingEdges minBy {case (t,h) => num.plus(distance(t),graph.weight(t,h))}
			explored add h
			distance(h) = num.plus(distance(t),graph.weight(t,h))
			outgoingEdges remove (t,h)
			outgoingEdges --= (outgoingEdges filter {case (_,node) => node == h})
			nextEdges = graph.adjacent(h) filterNot explored map (node => (h,node))
			outgoingEdges ++= nextEdges
		} while (!outgoingEdges.isEmpty && explored.size != nodesCount)
		distance
	}
}

