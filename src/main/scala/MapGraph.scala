import scalax.file.Path
import collection.mutable.{ArrayBuffer, Map, Queue}

class MapGraph[@specialized(Int) N](private val nodeMap:Map[N,Seq[N]]){

  def adjacentOf(node:N):Seq[N] = nodeMap(node)
  def adjacentOrUpdate(node:N,up: => Seq[N]):Seq[N] = nodeMap.getOrElseUpdate(node,up)

  def update(node:N,adjacent:Seq[N]) = nodeMap.put(node,adjacent)

  def has(node:N):Boolean =  nodeMap.contains(node)
  def nodes:Iterable[N] = nodeMap.keys

  def nodesCount:Int = nodeMap.size
  def edgesCount:Long = nodeMap.valuesIterator.foldLeft(0L){case (sum,adjacent) => sum + adjacent.size}

  def mergeNodes(mergedNode:N, removedNode:N):MapGraph[N] = {
    assert(has(mergedNode))
    assert(has(removedNode))
    new MapGraph(MapGraph.mergeNodes(nodeMap.clone(), mergedNode, removedNode))
  }
}

object MapGraph {

	def apply[@specialized(Int) N](nodes:(N,Seq[N])*):MapGraph[N] = new MapGraph(Map(nodes:_*))
	
	def readFromAdjacentListFile(path:Path):MapGraph[Int] = {
    val graph = MapGraph[Int]()
    for (line <-path.lines() if !line.trim.isEmpty) {
      val (node, adjacent) = readNodeAdjacentList(line)
      if(adjacent!=null) graph(node) = adjacent
    }
    graph
	}

	def readFromEdgeListFile(path:Path):MapGraph[Int] = {
		val graph = MapGraph[Int]()
		for (line <-path.lines() if !line.trim.isEmpty) {
      val i = line.indexOf(' ')
			val tail = line.substring(0,i).toInt
      val head = line.substring(i+1).trim.toInt
			val adjacent = graph.adjacentOrUpdate(tail,{new ArrayBuffer[Int]()}).asInstanceOf[ArrayBuffer[Int]] //safe
      adjacent += head
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

  def randomCutCount[@specialized(Int) N](graph:MapGraph[N]):Int = {
    val nodeMapClone = graph.nodeMap.clone()
    val nodesQueue = Queue[N](randomize(Seq[N](nodeMapClone.keySet.toSeq:_*)):_*)
    while(nodeMapClone.size>2){
      val node1 = nodesQueue.dequeue
      val adjacent = nodeMapClone(node1)
      if(adjacent.size>0){
        val j = (Math.random()*adjacent.size).asInstanceOf[Int]
        val node2 =  adjacent(j)
        mergeNodes(nodeMapClone,node2,node1)
      }
    }
    val (_, adjacent) = nodeMapClone.head
    adjacent.size
  }

	private def mergeNodes[@specialized(Int) N](nodeMap: Map[N, Seq[N]], mergedNode: N, removedNode: N):Map[N, Seq[N]] =  {
		//merge two adjacent lists, remove self-loops
		val removedAdjacent = nodeMap(removedNode)
    val mergedAdjacent = nodeMap(mergedNode)
		val newAdjacent = new ArrayBuffer[N](removedAdjacent.size+mergedAdjacent.size)
		for(node <- mergedAdjacent) {
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
