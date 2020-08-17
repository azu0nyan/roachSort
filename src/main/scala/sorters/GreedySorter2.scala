package sorters

import app.RoachSorter.{Roach, RoachSorter}

import scala.collection.mutable
import scala.language.implicitConversions

class GreedySorter2(val count:Int, val rouchesPerBatch:Int) extends RoachSorter(count, rouchesPerBatch){

  //  val allCombinations:Array[Array[Int]] = Array.ofDim[Int](math.pow(count, rouchesPerBatch).toInt, rouchesPerBatch)
  
  def calcInfoIfILessG(i:Roach, j:Roach):Int = {
    val elementsAtRight = Set(j) | r(j)
    val elementsAtLeft = Set(i) | l(i)
    elementsAtLeft.map(elementsAtRight &~ r(_)).map(_.size).sum + 
    elementsAtRight.map(elementsAtLeft &~ l(_)).map(_.size).sum 
  }
  
  def calcMinPriceForGroup(group:Array[Roach]):Int = {
    var sum = 0
    for(
      f <- 0 until group.length;
      s <- 0 until group.length
    ) {
      val i = group(f)
      val j = group(s)
      val priceIlessJ = calcInfoIfILessG(i, j)
      val priceJlessI =  calcInfoIfILessG(j, i)
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


