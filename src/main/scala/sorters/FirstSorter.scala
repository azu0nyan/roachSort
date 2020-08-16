package sorters

import app.Main.{count, rouchesPerBatch}
import app.RoachSorter._

import scala.collection.mutable


class FirstSorter(val count:Int, val rouchesPerBatch:Int) extends RoachSorter(count, rouchesPerBatch){
  
  def roachToSort: Set[Roach] = {
    val ret: mutable.Buffer[Roach] = mutable.Buffer()
    var candidats: Seq[Roach] = roachesSet.filter(placeUnknown).toSeq
    while (ret.size < elementsPerBatch && candidats.nonEmpty) {
      val sorted: Seq[Roach] = if (ret.isEmpty)
        candidats.sortBy(implicit x => left.size + right.size)
      else {
        candidats.sortBy(x => (ret.map( r => if(left(r).contains(x) || right(r).contains(x)) 1 else 0 ).sum, left(x).size + right(x).size))
        //          candidats.sortBy(x => ret.map( r => if(left(r).contains(x) || right(r).contains(x)) 1 else 0 ).sum)//if (ret.map(left(_)).contains(x) || ret.map(right(_)).contains(x)) 1 else 0)
      }
      //        println(candidats)
      candidats = sorted.tail
      ret += sorted.head
    }
    ret.toSet
  }
}
