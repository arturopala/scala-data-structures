import collection.mutable
import scalax.file.Path
import collection.mutable.{Map,Queue}

object Graph {

	def apply[@specialized(Int) T](nodes:(T,Seq[T])*):Graph[T] = Graph(Map(nodes:_*))
	
	def readFromFile(path:Path):Graph[Int] = {
		val nodes:Seq[(Int,Seq[Int])] = Seq((for (line <-path.lines().toSeq if !line.trim.isEmpty) yield readNode(line)):_*).filter(_!=null)
		Graph(Map(nodes:_*))
	}
	
	private def readNode(line:String):(Int,Seq[Int]) = {
		val tokens = line.split('\t')
		if(tokens.length==0) return null
		val label:Int = Integer.parseInt(tokens(0))
		val adjacent:Seq[Int] = Seq((for (i <- 1 until tokens.length) yield  Integer.parseInt(tokens(i))):_*)
		(label,adjacent)
	}

	private def mergeNodes[@specialized(Int) T](nodeMap: Map[T, Seq[T]], mergedNode: T, removedNode: T):Map[T, Seq[T]] =  {
		//merge two adjacent lists, remove self-loops
		val builder = mutable.LinearSeq.newBuilder[T]
		val removedAdjacent = nodeMap(removedNode)
		for(node <- nodeMap(mergedNode)) {
			if(node != mergedNode && node != removedNode) builder += node
		}
		for(node <- removedAdjacent) {
			if(node != mergedNode && node != removedNode) builder += node
		}
		val newAdjacent = builder.result
		nodeMap -= removedNode //remove node
		nodeMap(mergedNode) = newAdjacent //set new adjacent for mergedNode
		nodeMap transform {
			(_, adjacent) => adjacent map {
				case n if (n == removedNode) => mergedNode //replace all references to the merged node
				case n => n
			}
		}
		
		nodeMap
	}
	
	def randomize[T](seq:Seq[T]):Seq[T] =  {
		seq
			.map(item => (Math.random(),item))
			.sortBy{case (priority,_) => priority}
			.map{case (_,item) => item}
	}

}

case class Graph[@specialized(Int) T]( val nodeMap:Map[T,Seq[T]] ){
	
	def adjacentOf(node:T):Seq[T] = nodeMap(node)
	def has(node:T):Boolean =  nodeMap.contains(node)
	def nodes:Iterable[T] = nodeMap.keys
	def nodesCount:Int = nodeMap.size

	def mergeNodes(mergedNode:T, removedNode:T):Graph[T] = {
		assert(has(mergedNode))
		assert(has(removedNode))
		Graph(Graph.mergeNodes(nodeMap.clone(), mergedNode, removedNode))
	}
	
	def randomCutCount:Int = {
		val nodeMapClone = nodeMap.clone()
		val nodesQueue = Queue[T](Graph.randomize(nodeMapClone.keySet.toSeq):_*)
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
