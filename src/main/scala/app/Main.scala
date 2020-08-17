package app

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.util.Random
import RoachSorter._
import sorters.{FirstSorter, GreedySorter, GreedySorter2} 

object Main  {
  val threads = 12
  val count = 25
  val rouchesPerBatch = 5
  val maxi = 400
  val resultsEvery = 1

  def sort(roachPlaces: Seq[Roach], printLog: Boolean = false, sorter:RoachSorter, name:String): (Int, RoachSorter) = {
    var iter = 0
    if (printLog) println(roachPlaces)
    while (!sorter.allSorted) {
      iter += 1
      if (printLog) println(s"TURN $iter ${sorter.getClass}  $name")
      val toSort = sorter.roachToSort.toIndexedSeq
      if (printLog) println(toSort)
      val sorted = toSort.sortBy(roachPlaces)
      if (printLog) println(sorted)
      sorter.processComparasionResults(sorted)
      if (printLog) println(sorter.placingStr)
      if (printLog) println(sorter.roachInfoStr)
      sorter.validateState()
    }
    if (printLog) for (i <- 0 until count) print(sorter.placing(i).getOrElse(-1).toString + " ")
    (iter, sorter)
  }
  
  def main(args: Array[String]): Unit = {
//    sorters.nkIterator(25, 5).map(_.toSeq).foreach(println)
    runTestsMultithreaded(() => new GreedySorter2(count, rouchesPerBatch))
//    val arr = Array[Int](0, 1, 2)
//    println(arr.toSeq)
//    while(sorters.nextNK(arr, 25))println(arr.toSeq)    
  }
    
  def runTestsMultithreaded(supp:()=>RoachSorter): Unit = {
    @volatile var max = Integer.MIN_VALUE
    @volatile var min = Integer.MAX_VALUE
    val sorts = new mutable.HashMap[Int, Int]()

    var cur: AtomicInteger = new AtomicInteger(1)
    for (t <- 0 until threads) {
      new Thread(() => {
        while (cur.get() <= maxi) {
          val i = cur.getAndIncrement()
          val roachPlaces = i match {
            case 1 => (0 until count).toSeq
            case 2 => (0 until count).toSeq.reverse
            case _ => new Random(i).shuffle((0 until count))
          }
          val (turns, rs) = sort(roachPlaces, true, supp(), s"$i")
          if (rs.sorted != (0 until count).sortBy(roachPlaces)) {
            println(s"sorting not correct at seed $i")
            println(s"correct ${(0 until count).sortBy(roachPlaces).mkString(" ")}")
            println(s"sorting ${rs.sorted.mkString(" ")}")
          }
          this.synchronized {
            sorts.get(turns) match {
              case Some(value) => sorts(turns) = value + 1
              case None => sorts(turns) = 1
            }
            if (i % resultsEvery == 0) {
              println(s"Results at $i ")
              println(sorts.toSeq.sortBy(_._1).map { case (t, c) => f"$t%02d | $c%10d | ${c / i.toDouble}%.5f" }.mkString("\n"))
            }
            if (turns > max) {
              max = turns
              println(s"new max at seed ${i} $max")
            }
            if (turns < min) {
              min = turns
              println(s"new min at seed ${i} $min")
            }
          }
        }
      }).start()
    }
  }
}
