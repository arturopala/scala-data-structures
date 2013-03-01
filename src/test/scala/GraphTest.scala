import org.scalatest.FunSpec

class GraphTest extends FunSpec {

    describe("Graph"){

        val graph = Graph(
            1 -> Seq(2,3),
            2 -> Seq(3),
            3 -> Seq(4),
            4 -> Seq()
        )

        it("should have nodes and edges") {
            assert(graph.nodes.size==4, "graph nodes count should be 4")
            assert(graph.edges.size==4, "graph edges count should be 4")
        }
        it("should have reverse graph") {
            val reverse = graph.reverse
            val redges = reverse.edges.toSeq
            val reversed2 = reverse.reverse
            val radjacentOf1 = reverse.adjacent(1).toSeq
            val radjacentOf4 = reverse.adjacent(4).toSeq
            assert(reverse.nodes.size == 4, "reversed graph nodes count should be 4")
            assert(redges.size == 4, "reversed graph edges count should be 4")
            assert(reversed2 == graph, "twice reversed graph should be the same")
            assert(radjacentOf1.isEmpty)
            assert(radjacentOf4 == Seq(3))
        }
    }

}
