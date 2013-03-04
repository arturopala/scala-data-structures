import org.junit.Assert._
import org.scalatest.FunSpec
import scala.Predef._
import scalax.file.Path

class GraphTest extends FunSpec {

    describe("Graph"){

        val graph1 = Graph(
            1 -> Seq(2,3),
            2 -> Seq(3),
            3 -> Seq(4),
            4 -> Seq()
        )

        val graph2 = Graph(
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

        it("should have nodes and edges") {
            assert(graph1.nodes.size==4, "graph nodes count should be 4")
            assert(graph1.edges.size==4, "graph edges count should be 4")
        }
        it("should have reverse graph") {
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
            Graph.dfs(graph,new Graph.DfsLoopObserver[Int] {
                override def beforeInner(node:Int) {
                    counter = counter + 1
                }
            })(graph.nodes)
            assert(counter==graph.nodesCount)
        }
        it("should search graph with scc") {
            val graph = graph2
            val result = Graph.scc(graph)
            for((n,col) <- result.take(100)) Console.println(col.size+":"+col)
            assert(result.size==3)
        }
        /*it("should read SSC graph") {
            val graph = Graph.readFromEdgeListFile(Path.fromString("src/main/resources/SCC.txt"))
            assert(graph!=null)
            Console.println(graph.nodesCount)
            Console.println(graph.edgesCount)
            val result = Graph.scc(graph)
            for((n,col) <- result.take(100)) Console.println(col.size)
        }*/
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
    }

}
