import org.junit.Assert._
import org.junit.Test
import scalax.file.Path

class GraphTest {

	@Test def testReader = {
		val graph = Graph.readFromFile(Path.fromString("src/main/resources/graph1.txt"))
		assertEquals(200,graph.nodes.size)
	}
	
}
