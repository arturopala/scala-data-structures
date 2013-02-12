import scalax.file.Path

object Graph {
	
	def readFromFile(path:Path):Graph = {
		val nodes:Array[GraphNode] = (for (line <-path.lines() if !line.trim.isEmpty) yield parseNodeLine(line)).filter(_!=null).toArray
		Graph(nodes)
	}
	
	private def parseNodeLine(line:String):GraphNode = {
		val tokens = line.split('\t')
		if(tokens.length==0) return null
		val label:Int = Integer.parseInt(tokens(0))
		val adjacent:Array[Int] = (for (i <- 1 until tokens.length) yield  Integer.parseInt(tokens(i))).toArray
		GraphNode(label,adjacent)
	}

}

case class GraphNode(label:Int, adjacent:Array[Int])

case class Graph(nodes:Array[GraphNode])
