package org.encalmo.algorithms

import scala.specialized
import scalax.file.Path
import collection.mutable.{ArrayBuffer, Map => MutableMap, Seq => MutableSeq, HashMap, HashSet, Queue, Stack}

trait Graph[@specialized(Int) N] {
    def nodes: Traversable[N]
    def adjacent: N => Traversable[N]
    def edges: Traversable[(N,N)]
	def contains(node: N): Boolean
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
	override def contains(node: N): Boolean = nodes match {
		case s:Set[N] => s.contains(node)
		case _ => nodes exists (n => n==node)
	}
    override def reverse:Graph[N] = new GenericReverseGraph[N](self)
    override def nodesCount: Int = nodes.size
    override def edgesCount: Long = nodes.foldLeft(0L){case (sum,node) => sum + adjacent(node).size}
}

class GenericReverseGraph[@specialized(Int) N](origin: Graph[N]) extends GenericGraph[N] {
    override def nodes: Traversable[N] = origin.nodes
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
}

class MapAsGraph[@specialized(Int) N](val nodeMap: Map[N,Traversable[N]]) extends GenericGraph[N] {
	override val nodes: Iterable[N] =  nodeMap.keys
	override val adjacent: N => Traversable[N] = nodeMap
	override lazy val reverse: Graph[N] = Graph.hardCopyReversed[N](this)
}

class MutableGraph[@specialized(Int) N](
    val nodeMap: MutableMap[N,ArrayBuffer[N]] = new HashMap[N,ArrayBuffer[N]]()
) extends GenericGraph[N] {
    override def nodes:Iterable[N] =  nodeMap.keys
    override val adjacent: N => ArrayBuffer[N] = nodeMap
	override def reverse: Graph[N] = Graph.hardCopyReversed[N](this)
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
	
	def hardCopy[@specialized(Int) N](graph:Graph[N]): MutableGraph[N] =  new MutableGraph[N]().add(graph.edges)
	def hardCopyReversed[@specialized(Int) N](graph:Graph[N]): MutableGraph[N] =  {
		new MutableGraph[N](){
			override lazy val reverse: Graph[N] = graph
		}.addReverse(graph.edges)
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
		val nodeMap = MutableMap[Int,Traversable[Int]]()
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
		val nodeWeightMap = MutableMap[Int,Map[Int,Int]]()
		for (line <-path.lines() if !line.trim.isEmpty) {
			val (node, list) = parseNodeWeightAdjacentList(line)
			nodeWeightMap(node) = list
		}
		new WeightedGraphImpl[Int,Int](nodeWeightMap.keys, nodeWeightMap.mapValues{case m => m.keys}, (t:Int,h:Int) => nodeWeightMap(t)(h))
	}

    trait DfsVisitor[@specialized(Int) N] {
        def start(node:N) {}
        def before(node:N) {}
	    def edge(edge:(N,N)) {}
        def after(node:N) {}
    }

	/** Depth-first search of the whole graph */
	def dfs[@specialized(Int) N](graph:Graph[N], visitor: DfsVisitor[N]):Unit = dfs(graph, visitor, graph.nodes)
	/** Depth-first search of the whole graph in the given node's order*/
	def dfs[@specialized(Int) N](graph:Graph[N], visitor: DfsVisitor[N], nodes:Traversable[N]):Unit = {
		val explored = new HashSet[N]()
		for (node <- nodes){
			if (!(explored contains node)){
				visitor start node
				dfs2(graph,node,visitor,explored)
			}
		}
	}
	/** Depth-first search (recursive) of the graph starting at given node */
	def dfs[@specialized(Int) N](graph:Graph[N],node:N, visitor: DfsVisitor[N], explored:HashSet[N] = HashSet[N]()):Unit = {
		if (!(explored contains node)){
			explored add node
			visitor before node
			for (next <- graph.adjacent(node) if (!explored.contains(next))) {
				visitor edge ((node,next))
				dfs(graph,next,visitor,explored)
			}
			visitor after node
		}
	}

    /** Depth-first search (iterative) of the graph starting at given node */
    def dfs2[@specialized(Int) N](graph:Graph[N], source:N, visitor: DfsVisitor[N], explored:HashSet[N] = HashSet[N]()):Unit = {
        val stack = new Stack[N]()
        stack.push(source)
        visitor before source
        while (!stack.isEmpty){
            val node = stack.top
            graph.adjacent(node) find (n => !(explored.contains(n) || stack.contains(n))) match {
                case Some(next) => {
                    stack.push(next)
                    visitor before next
                    visitor edge ((node,next))
                }
                case None => {
                    stack.pop()
                    explored add node
                    visitor after node
                }
            }
        }
    }

	/** Breath-first search of the whole graph */
	def bfs[@specialized(Int) N](graph:Graph[N], visitor: N => Unit):Unit = {
		val explored = HashSet[N]()
		for (node <- graph.nodes){
			if (!(explored contains node)){
				bfs(graph,node,visitor,explored)
			}
		}
	}

	/** Breath-first search of the graph starting at given node */
	def bfs[@specialized(Int) N](graph:Graph[N], node: N, visitor: N => Unit, explored:HashSet[N] = HashSet[N]()):Unit = {
		val queue = new Queue[N]()
		queue.enqueue(node)
		while (!queue.isEmpty){
			val n = queue.dequeue
			if (!(explored contains n)){
				explored add n
				visitor(n)
				for (next <- graph.adjacent(n)) queue.enqueue(next)
			}
		}
	}

	def findCycles[@specialized(Int) N](graph:Graph[N]): Vector[N] = {
		var cycles: Vector[N] = Vector.empty[N]
		val marks = new HashMap[N,Char]().withDefaultValue('0')
		for (node <- graph.nodes if (marks(node) == '0')) {
			cycles = cycles ++ findCycles(graph,node,marks)
		}
		cycles
	}

	def findCycles[@specialized(Int) N](graph:Graph[N], node: N, marks: MutableMap[N,Char] = new HashMap[N,Char]().withDefaultValue('0')): Vector[N] = {
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
		val marks = new HashMap[N,Char]().withDefaultValue('0')
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
	
	def sortTopologically[@specialized(Int) N](graph: Graph[N]): List[N] = {
		var counter = graph.nodesCount
		var priorities: List[N] = Nil
		val observer = new DfsVisitor[N] {
			override def after(node: N) {
				priorities = node :: priorities
				counter = counter - 1
			}
		}
		dfs(graph,observer)
		priorities
	}

	/** Dijkstra algorithm finds shortest path in directed graph*/ 
	def findShortestPath[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N] with Weighted[N,V],from: N, to: N): (V,List[(N,N)]) = {
		findShortestPath(graph,from,to,graph.weight)
	}
	
	/** Dijkstra algorithm finds shortest path in directed graph */
	def findShortestPath[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N],from: N, to: N, weight: (N,N) => V): (V,List[(N,N)]) = {
		val num: Numeric[V] = implicitly[Numeric[V]]
		if(from==to || graph.adjacent(from).isEmpty) return (num.zero,Nil)
		val nodesCount = graph.nodesCount
		val explored = new HashSet[N]()
		val distance = new HashMap[N,V]()
		val backtrace = new MutableGraph[N]()
		implicit val ordering = new Ordering[(N,N,V)] {
			def compare(x: (N, N, V), y: (N, N, V)): Int =  {
				num.toInt(num.minus(num.plus(distance(x._1),x._3),num.plus(distance(y._1),y._3)))
			}
		}
		val outgoingEdges = new MinHeap[(N,N,V)](Math.min(graph.nodesCount,1024))
		var head = from
		explored add from
		distance(from) = num.zero
		var nextEdges = graph.adjacent(from) filterNot explored map (node => (from,node,weight(from,node)))
		outgoingEdges insert nextEdges
		do {
			for ((t,h,w) <- outgoingEdges.extract){
				explored add h
				distance(h) = num.plus(distance(t),w)
				backtrace add ((h,t))
				outgoingEdges remove (outgoingEdges filter {case (_,node,_) => node == h})
				nextEdges = graph.adjacent(h) filterNot explored map (node => (h,node,weight(h,node)))
				outgoingEdges insert nextEdges
				head = h
			}
		} while (head!=to && !outgoingEdges.isEmpty && explored.size != nodesCount)
		// compute resulting path
		var path: List[(N,N)] = Nil
		if(head==to){
			var next = to
			do {
				val node = next
				next = backtrace.adjacent(node).minBy(n => distance(n))
				val segment = (next,node)
				path = segment :: path
			} while(next!=from)
		}
		(distance(to), path)
	}

	/** Dijkstra algorithm finds all shortest paths starting at given node in directed graph */
	def findShortestPaths[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N] with Weighted[N,V],from: N): scala.collection.Map[N,V] = {
		findShortestPaths(graph,from,graph.weight)
	}

	/** Dijkstra algorithm finds all shortest paths starting at given node in directed graph */
	def findShortestPaths[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N],from: N, weight: (N,N) => V): scala.collection.Map[N,V] = {
		val num: Numeric[V] = implicitly[Numeric[V]]
		if(graph.adjacent(from).isEmpty) return Map.empty
		val nodesCount = graph.nodesCount
		val explored = new HashSet[N]()
		val distance = new HashMap[N,V]()
		implicit val ordering = new Ordering[(N,N,V)] {
			def compare(x: (N, N, V), y: (N, N, V)): Int =  {
				num.toInt(num.minus(num.plus(distance(x._1),x._3),num.plus(distance(y._1),y._3)))
			}
		}
		val outgoingEdges = new MinHeap[(N,N,V)](Math.min(graph.nodesCount,1024))
		var head = from
		explored add from
		distance(from) = num.zero
		var nextEdges = graph.adjacent(from) filterNot explored map (node => (from,node,weight(from,node)))
		outgoingEdges insert nextEdges
		do {
			for ((t,h,w) <- outgoingEdges.extract){
				explored add h
				distance(h) = num.plus(distance(t),w)
				outgoingEdges remove (outgoingEdges.view filter {case (_,node,_) => node == h})
				nextEdges = graph.adjacent(h) filterNot explored map (node => (h,node,weight(h,node)))
				outgoingEdges insert nextEdges
				head = h
			}
		} while (!outgoingEdges.isEmpty && explored.size != nodesCount)
		distance
	}

	/* Kosaraju's 2-dfs pass algorithm finds strongly connected components */
	def findStronglyConnectedComponents[@specialized(Int) N](graph:Graph[N]): Iterable[Iterable[N]] = {
		class SccNodeInfo {
			var leader:Option[N] = None
			var time:Int = 0
		}
		val attributes = new HashMap[N,SccNodeInfo]()
		def attrOf(node:N):SccNodeInfo = attributes.getOrElseUpdate(node,{new SccNodeInfo})
		val reversed: Graph[N] = graph.reverse
		var t:Int = 0
		var s: Option[N] = None
		// first dfs pass
		val observer1 = new DfsVisitor[N] {
			override def after(node:N) {
				t = t + 1
				attrOf(node).time = t
			}
		}
		dfs(reversed,observer1)
		// nodes sorting by reverse time
		val times: MutableSeq[(N,Int)] = MutableSeq() ++ (attributes.view map {case (node,attr) => (node,attr.time)})
		implicit val ordering = new Ordering[(N,Int)] {
			def compare(x: (N, Int), y: (N, Int)): Int = y._2 - x._2
		}
		QuickSort.sort(times)
		val ordered = times.view map {case (node,_) => node}
		val observer2 = new DfsVisitor[N] {
			override def start(node:N) {
				s = Some(node)
			}
			override def before(node:N) {
				attrOf(node).leader = s
			}
		}
		// second dfs pass
		dfs(graph,observer2,ordered)
		// result computing
		val result = (attributes groupBy {case (_,attr) => attr.leader.get}).toSeq sortBy {case (_,map) => -map.size} map {case (n,map) => map.keys}
		result
	}

	def mergeNodes[@specialized(Int) N](g: Graph[N], mergedNode: N, removedNode: N): MutableGraph[N] =  {
		val graph:MutableGraph[N] = g match {
			case x: MutableGraph[N] => x
			case _ => Graph.hardCopy(g)
		}
		//merge two adjacent lists, remove self-loops
		val removedAdjacent = graph.nodeMap(removedNode)
		val mergedAdjacent = graph.nodeMap(mergedNode)
		val newAdjacent = new ArrayBuffer[N](removedAdjacent.size+mergedAdjacent.size)
		for(node <- mergedAdjacent) {
			if(node != removedNode) newAdjacent += node
		}
		for(node <- removedAdjacent) {
			if(node != mergedNode) newAdjacent += node
		}
		graph.nodeMap -= removedNode //remove node
		graph.nodeMap(mergedNode) = newAdjacent //set new adjacent for mergedNode
		graph.nodeMap transform {
			(_, adjacent) => {
				if (adjacent.contains(removedNode)){
					adjacent map {
						case n if n==removedNode => mergedNode
						case n => n
					}
				} else {
					adjacent
				}
			}
		}
		graph
	}

	def randomize[N](seq:Seq[N]):Seq[N] =  {
		seq
			.map (item => (Math.random(),item))
			.sortBy {case (priority,_) => priority}
			.map {case (_,item) => item}
	}

	def randomCutCount[@specialized(Int) N](g:Graph[N]):Int = {
		val graph:MutableGraph[N] = g match {
			case x: MutableGraph[N] => x
			case _ => Graph.hardCopy(g)
		}
		val nodesQueue = Queue[N](randomize(graph.nodes.toSeq):_*)
		while(graph.nodeMap.size>2){
			val node1 = nodesQueue.dequeue
			val adjacent = graph.nodeMap(node1)
			if(adjacent.size>0){
				val j = (Math.random()*adjacent.size).asInstanceOf[Int]
				val node2 =  adjacent(j)
				mergeNodes(graph,node2,node1)
			}
		}
		val (_, adjacent) = graph.nodeMap.head
		adjacent.size
	}
}

