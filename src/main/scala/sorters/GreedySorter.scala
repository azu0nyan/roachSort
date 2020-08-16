package sorters

import app.RoachSorter.{Roach, RoachSorter}

import scala.collection.mutable
import scala.language.implicitConversions

class GreedySorter(val count:Int, val rouchesPerBatch:Int) extends RoachSorter(count, rouchesPerBatch){

//  val allCombinations:Array[Array[Int]] = Array.ofDim[Int](math.pow(count, rouchesPerBatch).toInt, rouchesPerBatch)

  def calcMinPriceForGroup(group:Array[Roach]):Int = {
    var sum = 0
    for(
      f <- 0 until group.length;
      s <- 0 until group.length
    ) {
      val i = group(f)
      val j = group(s)
      val priceIlessJ = ((Set(i)  | left(i))  &~ left(j)).size +
        ((Set(j) | right(j)) &~ right(i)).size
      val priceJlessI = ((Set(j)  | left(j))  &~ left(i)).size +
        ((Set(i) | right(i)) &~ right(j)).size
      sum += math.min(priceIlessJ, priceJlessI)
    }
    sum 
  }

  def roachToSort: Set[Roach] = {
    var candidats: IndexedSeq[Roach] = roachesSet.filter(placeUnknown).toIndexedSeq
    if(candidats.size <= rouchesPerBatch){
      candidats.toSet   
    } else {
      sorters.nkIterator(candidats.size, rouchesPerBatch).map(_.map(candidats)).maxBy(calcMinPriceForGroup).toSet      
    }    
  }
}

