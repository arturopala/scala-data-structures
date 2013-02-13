import scalax.file.Path
import scala.collection.mutable.Map

object Graph {

	def apply[@specialized(Int) T](nodes:(T,Seq[T])*) = new Graph(Map(nodes:_*))
	
	def readFromFile(path:Path):Graph[Int] = {
		val nodes:Seq[(Int,Seq[Int])] = (for (line <-path.lines() if !line.trim.isEmpty) yield readNode(line)).filter(_!=null).toSeq
		Graph(Map(nodes:_*))
	}
	
	private def readNode(line:String):(Int,Seq[Int]) = {
		val tokens = line.split('\t')
		if(tokens.length==0) return null
		val key:Int = Integer.parseInt(tokens(0))
		val adjacent:Seq[Int] = for (i <- 1 until tokens.length) yield  Integer.parseInt(tokens(i))
		(key,adjacent)
	}

	private def mergeNodes[T](nodeMap: Map[T, Seq[T]], mergedNode: T, removedNode: T):Map[T, Seq[T]] =  {
		//merge two adjacent lists, remove self-loops
		val newAdjacent = (nodeMap(mergedNode) ++ nodeMap(removedNode)) filter (node => node != mergedNode && node != removedNode)
		nodeMap -= removedNode //remove merged node
		nodeMap -= mergedNode //remove target node
		nodeMap transform {
			(node, adjacent) => adjacent map {
				case n if (n == removedNode) => mergedNode //replace all references to the merged node
				case n => n
			}
		}
		nodeMap(mergedNode) = newAdjacent
		nodeMap
	}

}

case class Graph[@specialized(Int) T](private val nodeMap:Map[T,Seq[T]]){
	
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
		while(nodeMapClone.size>2){
			val (node1,adjacent) = nodeMapClone.head
			if(adjacent.size>0){
				val j = (Math.random()*adjacent.size).asInstanceOf[Int]
				val node2 =  adjacent(j)
				Graph.mergeNodes(nodeMapClone,node1,node2)
			}
		}
		val (_, adjacent) = nodeMapClone.head
		adjacent.size
	}
}
