import java.util.concurrent.atomic.AtomicInteger

import RoachSorter._

import scala.util.Random

object Main extends App {
  val threads = 12
  val count = 25
  val rouchesPerBatch = 5


  def sort(roachPlaces: Seq[Roach], printLog: Boolean = false): (Int, RoachSorter) = {
    val sorter = new RoachSorter(count, rouchesPerBatch)
    var iter = 0
    if (printLog) println(roachPlaces)
    while (!sorter.allSorted) {
      iter += 1
      if (printLog) println(s"TURN $iter")
      val toSort = sorter.roachToSort.toIndexedSeq
      if (printLog) println(toSort)
      val sorted = toSort.sortBy(roachPlaces)
      if (printLog) println(sorted)
      sorter.processComparasionResults(sorted)
      if (printLog) println(sorter.placingStr)
      if (printLog) println(sorter.roachInfoStr)
      sorter.validateState()
    }
    if (printLog) for (i <- 0 to 24) print(sorter.placing(i).getOrElse(-1).toString + " ")
    (iter, sorter)
  }

  @volatile var max = Integer.MIN_VALUE
  @volatile var min = Integer.MAX_VALUE
  var cur: AtomicInteger = new AtomicInteger(0)
  for (t <- 0 until threads) {
    new Thread(() => {
      while (true) {
        val i = cur.getAndIncrement()
        val roachPlaces = i match {
          case 0 => (0 until count).toSeq
          case 1 => (0 until count).toSeq.reverse
          case _ => new Random(i).shuffle((0 until count))
        }
        val (turns, rs) = sort(roachPlaces, false)
        if (rs.sorted != (0 until count).sortBy(roachPlaces)) {
          println(s"sorting not correct at seed $i")
          println(s"correct ${(0 until count).sortBy(roachPlaces).mkString(" ")}")
          println(s"sorting ${rs.sorted.mkString(" ")}")
        }
        this.synchronized {
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
