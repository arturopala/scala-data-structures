import org.junit.Assert._
import org.junit.Test
import scalax.file.Path

class GraphTest {

	val graph1 = Graph[Int](
		0 -> Seq(1,3),
		1 -> Seq(0,2,3),
		2 -> Seq(1,3),
		3 -> Seq(0,1,2)
	)

	val graph2 = Graph[Int](
		0 -> Seq(1,2,3),
		1 -> Seq(0,2,3),
		2 -> Seq(0,1,3),
		3 -> Seq(0,1,2)
	)
	
	val graph = Graph.readFromFile(Path.fromString("src/main/resources/graph1.txt"))

	@Test def testReader = {
		assertEquals(200,graph.nodesCount)
	}

	@Test def testMergeNodes = {
		val graph2 = graph1.mergeNodes(1,0)
		assertFalse(graph2.has(0))
		assertEquals(Set(1,2,3),graph2.nodes)
		assertEquals(Seq(2,3,3),graph2.adjacentOf(1))
		assertEquals(Seq(1,3),graph2.adjacentOf(2))
		assertEquals(Seq(1,1,2),graph2.adjacentOf(3))
		val graph3 = graph2.mergeNodes(2,1)
		assertFalse(graph3.has(1))
		assertEquals(Set(2,3),graph3.nodes)
		assertEquals(Seq(3,3,3),graph3.adjacentOf(2))
		assertEquals(Seq(2,2,2),graph3.adjacentOf(3))
		val graph4 = graph3.mergeNodes(3,2)
		assertFalse(graph4.has(2))
		assertEquals(Set(3),graph4.nodes)
		assertEquals(Seq.empty,graph4.adjacentOf(3))
		Console.println(graph1)
		Console.println(graph2)
		Console.println(graph3)
		Console.println(graph4)
	}

	@Test def testRandomCutCount = {
		for(i <- 1 to 10){
			Console.println(graph1.randomCutCount)
		}
	}

	@Test def findMinCutCount = {
		var count = Integer.MAX_VALUE
		for(i <- 1 to graph.nodesCount){
			count = Math.min(graph.randomCutCount,count)
		}
		Console.println(count)
	}
	
}
