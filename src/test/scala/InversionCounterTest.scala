import org.junit.Assert._
import org.junit.Test
import scalax.file.Path

class InversionCounterTest {
  
  @Test def test = {
    val (s1,c1) = InversionCounter.sortAndCountInversions(Seq("1","2","3","4","5","6"))
    assertEquals(0,c1)
    val (s2,c2) = InversionCounter.sortAndCountInversions(Seq("2","1"))
    assertEquals(1,c2)
    val (s3,c3) = InversionCounter.sortAndCountInversions(Seq("3","2","1"))
    assertEquals(3,c3)
    val (s4,c4) = InversionCounter.sortAndCountInversions(Seq("4","3","2","1"))
    assertEquals(6,c4)
    val (s5,c5) = InversionCounter.sortAndCountInversions(Seq("1","2","3","4","6","5"))
    assertEquals(Seq("1","2","3","4","5","6"),s5)
    assertEquals(1,c5)
    val (s6,c6) = InversionCounter.sortAndCountInversions(Seq("4","6","7","5"))
    assertEquals(2,c6)
    val (s7,c7) = InversionCounter.sortAndCountInversions(Seq("1","2","3","4","6","7","5"))
    assertEquals(Seq("1","2","3","4","5","6","7"),s7)
    assertEquals(2,c7)
    val (s8,c8) = InversionCounter.mergeAndCountSplitInversions(Seq("5"),Seq("4"))
    assertEquals(Seq("4","5"),s8)
    assertEquals(1,c8)
  }
  
  @Test def countHomework = {
    val input:Seq[Integer] = Path.fromString("src/main/resources/inversions.txt").lines().map(Integer.valueOf(_)).toIndexedSeq
    val (sorted,count) = InversionCounter.sortAndCountInversions(input)
    Console.print(count)
  }
  
  
}