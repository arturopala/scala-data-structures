import org.scalatest.FunSpec
import scala.Predef._
import scalax.file.Path

class GraphTest extends FunSpec {

    describe("Graph"){

        val graph1 = Graph[Int](
            1 -> Seq(2,3),
            2 -> Seq(3),
            3 -> Seq(4),
            4 -> Seq()
        )

	    /* cyclic, connected components */
        val graph2 = Graph[Int](
            1 -> Seq(2,3,6),
            2 -> Seq(3),
            3 -> Seq(4),
            4 -> Seq(5,4),
            5 -> Seq(1,3),
            6 -> Seq(7,10),
            7 -> Seq(8),
            8 -> Seq(9,10),
            9 -> Seq(6),
            10 -> Seq(6,7),
            11 -> Seq(12,7),
            12 -> Seq(13),
            13 -> Seq(11)
        )

	    /* acyclic, weighted */
	    val graph3 = Graph[Int,Int](
		    1 -> Seq((2,1),(3,2),(4,3)),
		    2 -> Seq((3,3),(5,4)),
		    3 -> Seq((4,2),(5,2)),
		    4 -> Seq((5,3)),
		    5 -> Seq()
	    )

	    /* acyclic */
	    val graph4 = Graph[Int](
		    5 -> Seq(2,3,4,6),
		    2 -> Seq(3,6),
		    3 -> Seq(6),
		    4 -> Seq(2),
	        6 -> Seq()
	    )

        it("should have nodes and edges") {
            assert(graph1.nodes.size==4, "graph nodes count should be 4")
            assert(graph1.edges.size==4, "graph edges count should be 4")
        }
        it("should have copyReversed graph") {
            val reverse = graph1.reverse
            val redges = reverse.edges.toSeq
            val reversed2 = reverse.reverse
            val radjacentOf1 = reverse.adjacent(1).toSeq
            val radjacentOf4 = reverse.adjacent(4).toSeq
            assert(reverse.nodes.size == 4, "reversed graph nodes count should be 4")
            assert(redges.size == 4, "reversed graph edges count should be 4")
            assert(reversed2 == graph1, "twice reversed graph should be the same")
            assert(radjacentOf1.isEmpty)
            assert(radjacentOf4 == Seq(3))
        }
        it("should search graph with dfs") {
            val graph = graph2
            var counter = 0
            Graph.dfs(graph,new Graph.Observer[Int] {
                override def before(node:Int) {
                    counter = counter + 1
                }
            })
            assert(counter==graph.nodesCount)
        }
        it("should search graph with findStronglyConnectedComponents") {
            val graph = graph2
            val result = Graph.findStronglyConnectedComponents(graph)
            assert(result.size==3)
        }
	    it("should read adjacent list graph from file") {
		    val graph = Graph.readFromAdjacentListFile(Path.fromString("src/main/resources/graph1.txt"))
		    assert(graph.nodesCount==200)
		    assert(graph.adjacent(82).size==27)
	    }
	    it("should read adjacent-weight list graph from file") {
		    val graph = Graph.readFromAdjacentWeightListFile(Path.fromString("src/main/resources/dijkstraData.txt"))
		    assert(graph.nodesCount==200)
		    assert(graph.weight(200,108)==9976)
		    assert(graph.adjacent(31).size==21)
	    }
	    it("should find cycles - graph2") {
		    val cycles = Graph.findCycles(graph2)
		    assert(cycles.size == 6)
	    }
	    it("should find cycles - graph3") {
		    val cycles = Graph.findCycles(graph3)
		    assert(cycles.isEmpty)
	    }
	    it("should check cycles") {
		    assert(Graph.hasCycles(graph2))
		    assert(!Graph.hasCycles(graph3))
	    }
	    it("should sort topologically - graph3") {
		    val order = Graph.sortTopologically(graph3)
		    assert(order.sameElements(Seq(1, 2, 3, 4, 5)))
	    }
	    it("should sort topologically - graph4") {
		    val order = Graph.sortTopologically(graph4)
		    assert(order.sameElements(Seq(5, 4, 2, 3, 6)))
	    }
	    it("should compute shortest path - graph3") {
		    val (distance,path) = Graph.findShortestPath(graph3,1,5)
		    assert(distance==4)
		    assert(path==List((1,3), (3,5)))
	    }
	    it("should compute shortest path - dijkstraData") {
		    val graph = Graph.readFromAdjacentWeightListFile(Path.fromString("src/main/resources/dijkstraData.txt"))
		    assert(graph.nodesCount==200)
		    assert(graph.weight(200,108)==9976)
		    assert(graph.adjacent(31).size==21)
		    val path1 = Graph.findShortestPath(graph,1,197)
		    assert(path1==(3068,List((1,114), (114,103), (103,110), (110,197))))
		    val path2 = Graph.findShortestPath(graph,1,115)
		    assert(path2==(2399,List((1,80), (80,115))))
	    }
	    it("should compute all shortest paths - graph3") {
		    val distance = Graph.findShortestPaths(graph3,1)
		    assert(distance.size==5)
		    assert(distance==Map(1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4))
	    }
	    it("should compute all shortest paths - dijkstraData") {
		    val graph = Graph.readFromAdjacentWeightListFile(Path.fromString("src/main/resources/dijkstraData.txt"))
		    assert(graph.nodesCount==200)
		    assert(graph.weight(200,108)==9976)
		    assert(graph.adjacent(31).size==21)
		    val distance = Graph.findShortestPaths(graph,1)
		    val nodes = Seq(7,37,59,82,99,115,133,165,188,197)
		    val result = nodes map distance
		    assert(result==List(2599, 2610, 2947, 2052, 2367, 2399, 2029, 2442, 2505, 3068))
	    }
	    /*it("should find strongly connected components - scc") {
			val graph = Graph.readFromEdgeListFile(Path.fromString("src/main/resources/SCC.txt"))
			assert(graph!=null)
			Console.println(graph.nodesCount)
			Console.println(graph.edgesCount)
			val result = Graph.findStronglyConnectedComponents(graph)
			for(scc <- result.take(100)) Console.println(scc.size)
		}*/
    }

}
