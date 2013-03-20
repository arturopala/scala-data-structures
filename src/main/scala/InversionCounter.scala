package org.encalmo.algorithms

object InversionCounter {
 
  
  def sortAndCountInversions[T <: Comparable[T]](input:Seq[T]):(Seq[T],Long) = {
     if (input.size == 1) return (input,0)
     val (left, right) = input.splitAt(input.size/2)
     val (leftSorted,leftCount) = sortAndCountInversions(left)
     val (rightSorted,rightCount) = sortAndCountInversions(right)
     val (merged,splitCount) = mergeAndCountSplitInversions[T](leftSorted,rightSorted)
     (merged,leftCount + rightCount + splitCount)
  }
  
  def mergeAndCountSplitInversions[T <: Comparable[T]](left:Seq[T], right:Seq[T]):(Seq[T],Long) = {
       val merged = new scala.collection.mutable.ArraySeq[T](left.size + right.size)
       var counter:Long = 0
       var i = 0
       var j = 0
       for (x <- 0 until merged.size){
           if (i == left.size){
             merged(x) = right(j)
             j = j + 1
           } else 
           if (j == right.size){
             merged(x) = left(i)
             i = i + 1
           } else {
             if (left(i).compareTo(right(j)) <= 0){
                merged(x) = left(i)
                i = i + 1
             } else {
               counter = counter +  (left.size-i)
               merged(x) = right(j)
               j = j + 1
             }
           }
       }
       (merged,counter)
  }

}





