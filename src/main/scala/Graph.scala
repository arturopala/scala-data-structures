import scalax.file.Path
import collection.mutable.{ArrayBuffer, Map, Queue}

object Graph {

	def apply[@specialized(Int) N](nodes:(N,Seq[N])*):Graph[N] = new Graph(Map(nodes:_*))
	
	def readFromAdjacentListFile(path:Path):Graph[Int] = {
		val nodes:Seq[(Int,Seq[Int])] = Seq((for (line <-path.lines().toSeq if !line.trim.isEmpty) yield readNodeAdjacentList(line)):_*).filter(_!=null)
		Graph(Map(nodes:_*))
	}

	def readFromEdgeFile(path:Path):Graph[Int] = {
		val graph = Graph[Int]()
		for (line <-path.lines().toSeq if !line.trim.isEmpty) {
			val edge = line.split(' ') map (_.toInt)
			graph.adjacentOf(edge(0))
		}
		graph
	}
	
	private def readNodeAdjacentList(line:String):(Int,Seq[Int]) = {
		val tokens = line.split('\t')
		if(tokens.length==0) return null
		val label:Int = Integer.parseInt(tokens(0))
		val adjacent:Seq[Int] = Seq((for (i <- 1 until tokens.length) yield  Integer.parseInt(tokens(i))):_*)
		(label,adjacent)
	}

	private def mergeNodes[@specialized(Int) N](nodeMap: Map[N, Seq[N]], mergedNode: N, removedNode: N):Map[N, Seq[N]] =  {
		//merge two adjacent lists, remove self-loops
		val removedAdjacent = nodeMap(removedNode)
		val newAdjacent = ArrayBuffer[N]()
		for(node <- nodeMap(mergedNode)) {
			if(node != removedNode) newAdjacent += node
		}
		for(node <- removedAdjacent) {
			if(node != mergedNode) newAdjacent += node
		}
		nodeMap -= removedNode //remove node
		nodeMap(mergedNode) = newAdjacent //set new adjacent for mergedNode
		nodeMap transform {
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
		nodeMap
	}
	
	def randomize[N](seq:Seq[N]):Seq[N] =  {
		seq
			.map(item => (Math.random(),item))
			.sortBy{case (priority,_) => priority}
			.map{case (_,item) => item}
	}

}

case class Graph[@specialized(Int) N]( val nodeMap:Map[N,Seq[N]] ){
	
	def adjacentOf(node:N):Seq[N] = nodeMap(node)
	def has(node:N):Boolean =  nodeMap.contains(node)
	def nodes:Iterable[N] = nodeMap.keys
	def nodesCount:Int = nodeMap.size

	def mergeNodes(mergedNode:N, removedNode:N):Graph[N] = {
		assert(has(mergedNode))
		assert(has(removedNode))
		Graph(Graph.mergeNodes(nodeMap.clone(), mergedNode, removedNode))
	}
	
	def randomCutCount:Int = {
		val nodeMapClone = nodeMap.clone()
		val nodesQueue = Queue[N](Graph.randomize(Seq[N](nodeMapClone.keySet.toSeq:_*)):_*)
		while(nodeMapClone.size>2){
			val node1 = nodesQueue.dequeue
			val adjacent = nodeMapClone(node1)
			if(adjacent.size>0){
				val j = (Math.random()*adjacent.size).asInstanceOf[Int]
				val node2 =  adjacent(j)
				Graph.mergeNodes(nodeMapClone,node2,node1)
			}
		}
		val (_, adjacent) = nodeMapClone.head
		adjacent.size
	}
}
