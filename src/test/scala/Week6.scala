import org.scalatest.FunSpec
import scalax.file.Path
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

class Week6 extends FunSpec {
		
	/*describe("assignment 6.1"){
		it("should compute the number of target values t in the interval [2500,4000] (inclusive) " +
				"such that there are distinct numbers x,y in the input file that satisfy x+y=t."){
			val map = new HashMap[Int,Int]()
			val path = Path.fromString("src/main/resources/HashInt.txt")
			for (line <-path.lines() if !line.trim.isEmpty) {
				val num = line.trim.toInt
				map.get(num) match {
					case None => map(num) = 1
					case Some(x) => map(num) = x+1
				}
			}
			val set = new HashSet[Int]()
			for (x <- map.keys){
				for(t <- 2500 to 4000){
				   val y = t - x
				   if(y > 0 && y != x){
					   map.get(y) match {
						   case Some(n) => set add t
						   case None =>
					   }
				   }
				}
			}
			Console.println(s"Result is ${set.size}")
		}
	} */

}
