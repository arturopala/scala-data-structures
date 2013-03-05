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

class MutableMapGraph[@specialized(Int) N] extends GenericGraph[N] {
    protected val nodeMap = scala.collection.mutable.Map[N,ArrayBuffer[N]]()
    override def nodes:Iterable[N] =  nodeMap.keys
    override val adjacent: N => ArrayBuffer[N] = nodeMap.withDefaultValue(ArrayBuffer.empty[N])
	override def reverse = Graph.reverse[N](this)
	override def nodesCount: Int = nodeMap.size
	
	def add(node:N, adjacent:ArrayBuffer[N]):MutableMapGraph[N] = {
		nodeMap.update(node,adjacent); this
	} 
    def add(edge: (N,N)):MutableMapGraph[N] = {
        nodeMap.getOrElseUpdate(edge._1,{ArrayBuffer[N]()}) += (edge._2); this
    }
    def add(edges:Traversable[(N,N)]):MutableMapGraph[N] = {
        for (edge <- edges) add(edge); this
    }
    def addReverse(edge: (N,N)):MutableMapGraph[N] = {
        nodeMap.getOrElseUpdate(edge._2,{ArrayBuffer[N]()}) += (edge._1); this
    }
    def addReverse(edges:Traversable[(N,N)]):MutableMapGraph[N] = {
        for (edge <- edges) addReverse(edge); this
    }
}

object Graph {
	
	class GenericGraphImpl[@specialized(Int) N](val nodes:Iterable[N], val adjacent: N => Traversable[N]) extends GenericGraph[N]
	class WeightedGraphImpl[@specialized(Int) N, @specialized(Double,Int) V:Numeric](val nodes:Iterable[N], val adjacent: N => Traversable[N], val weight: (N,N) => V) extends GenericGraph[N] with Weighted[N,V]

    def apply[@specialized(Int) N](mappings:(N,Iterable[N])*): Graph[N] = {
	    val nodeMap = mappings.toMap
	    new GenericGraphImpl[N](nodeMap.keys,nodeMap.withDefaultValue(Traversable.empty[N]))
    }
	def apply[@specialized(Int) N](nodes:Iterable[N], adjacent: N => Traversable[N]): Graph[N] = new GenericGraphImpl[N](nodes,adjacent)
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
		new GenericGraphImpl[Int](nodeMap.keys, nodeMap.withDefaultValue(Traversable.empty[Int]))
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

    def reverse[N](graph:Graph[N]):Graph[N] =  new MutableMapGraph[N](){
        override lazy val reverse = graph
    }.addReverse(graph.edges)

    trait Observer[N] {
        def start(node:N) {}
        def before(node:N) {}
        def after(node:N) {}
    }

    class SccNodeInfo[N] {
        var leader:Option[N] = None
        var time:Int = 0
    }

    /** Kosaraju's two dfs pass algorithm finds strongly connected components */
    def scc[@specialized(Int) N](graph:Graph[N]): Iterable[Iterable[N]] = {
        //first pass
        val reversed: Graph[N] = graph.reverse
        val attributes = mutable.Map[N,SccNodeInfo[N]]()
        def attrOf(node:N):SccNodeInfo[N] = attributes.getOrElseUpdate(node,{new SccNodeInfo[N]})
        var t:Int = 0
        var s: Option[N] = None
        val observer1 = new Observer[N] {
            override def after(node:N) {
                t = t + 1
                attrOf(node).time = t
            }
        }
        dfs(reversed,observer1)(reversed.nodes)
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
        dfs(graph,observer2)(ordered)
        //result
        val result = (attributes groupBy {case (_,attr) => attr.leader.get}).toSeq sortBy {case (_,map) => -map.size} map {case (n,map) => map.keys}
        result
    }

    def dfs[@specialized(Int) N](graph:Graph[N], observer: Observer[N])(nodes:Iterable[N] = graph.nodes):Unit = {
        val explored = mutable.HashSet[N]()
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
                dfs(graph,n,observer,explored)
            }
            observer after node
        }
    }
	
	/** Dijkstra algorithm finds shortest path */ 
	def shortestPath[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N] with Weighted[N,V],from: N, to: N): Iterable[((N,N),V)] = {
		if(from==to || graph.adjacent(from).isEmpty) return Seq.empty[((N,N),V)]
		val num: Numeric[V] = implicitly[Numeric[V]]
		val explored = mutable.HashSet[N]()
		val distance = mutable.Map[N,V]()
		val outgoingEdges = mutable.HashSet[(N,N)]()
		explored add from
		distance(from) = num.zero
		outgoingEdges ++= (graph.adjacent(from) map (h => (from,h)))
		var counter = 2
		Console.println("nodes: "+graph.nodesCount+" edges: "+graph.edgesCount)
		var head = from
		while(head!=to && !outgoingEdges.isEmpty && explored.size != graph.nodesCount){
			for((t,h) <- outgoingEdges){
				distance(h) = distance.get(h) match {
					case None => num.plus(distance(t),graph.weight(t,h))
					case Some(v) => num.min(v,num.plus(distance(t),graph.weight(t,h)))
				}
			}
			val (t,h) = outgoingEdges minBy {case (_,h) => distance(h)}
			Console.println(counter+". "+t+" -> "+h+" : "+distance(h)+" ("+outgoingEdges.size+")")
			explored add h
			outgoingEdges remove (t,h)
			outgoingEdges --= (outgoingEdges filter {case (_,n) => n == h})
			outgoingEdges ++= (graph.adjacent(h) filterNot explored map (n => (h,n)))
			head = h
			counter = counter + 1
		}
		Console.println(distance(to))
		Nil
	}
}

