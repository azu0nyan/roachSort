package app

import scala.collection.mutable

import scala.language.postfixOps

object RoachSorter {
  type Roach = Int

  abstract class RoachSorter(val rouchesTotal: Int = 25, val elementsPerBatch: Int = 5) {

    def validateState() = {
      for (i <- 0 until rouchesTotal) {
        given Roach = i
        assert((left & right) isEmpty)
        assert(!(left.contains(i)))
        assert(!(right.contains(i)))
        assert((left & right).size < rouchesTotal)

        val inLeft: Set[Roach] = roaches.filter(left(_).contains(i)).toSet
        val inRight: Set[Roach] = roaches.filter(right(_).contains(i)).toSet
        if (left != inRight) println(s"$i left : $left inRight: $inRight diff l\\r ${left &~ inRight} r\\l ${inRight &~ left}")
        if (right != inLeft) println(s"$i  right : $right inLeft: $inLeft")
        assert(left == inRight)
        assert(right == inLeft)
      }
    }

    val roaches: Seq[Roach] = (0 until rouchesTotal).toSeq

    val roachesSet: Set[Roach] = (0 until rouchesTotal).toSet

    val placing: Array[Option[Int]] = Array.fill[Option[Int]](rouchesTotal)(None)

    val l = Array.fill[Set[Roach]](rouchesTotal)(Set())
    val r = Array.fill[Set[Roach]](rouchesTotal)(Set())


    def left(implicit roach: Roach): Set[Roach] = l(roach)
    def right(implicit roach: Roach): Set[Roach] = r(roach)

    def isPlaceKnown(implicit roach: Roach): Boolean = placing(roach).nonEmpty

    def placeUnknown(implicit roach: Roach): Boolean = !isPlaceKnown

    def isAllLRKnown(implicit roach: Roach): Boolean = (left | right).size == rouchesTotal - 1

    def unplaced: Set[Roach]  = roachesSet.filter(!isPlaceKnown(_))

    def place(implicit roach: Roach): Unit = placing(roach) = Some(left.size)

    def processComparasionResults(res: IndexedSeq[Roach]): Unit =
      for (
        i <- 0 until res.size;
        j <- (i + 1) until res.size
      ) {
        val leftmost = res(i)
        val rightmost = res(j)
        val elementsAtRight = Set(rightmost) | r(rightmost)
        val elementsAtLeft = Set(leftmost) | l(leftmost)
        elementsAtLeft.foreach(r(_) ++= elementsAtRight)
        elementsAtRight.foreach(l(_) ++= elementsAtLeft)
        /*r(leftmost) += rightmost
        l(rightmost) += leftmost
        r(leftmost) ++= right(rightmost)
        for (rn <- right(rightmost)) l(rn) += leftmost
        l(rightmost) ++= left(leftmost)
        for (ln <- left(leftmost)) {
          r(ln) += rightmost
          
        }*/

      }
      unplaced.filter(isAllLRKnown).foreach(place)

    def roachToSort: Set[Roach] //= (unplaced.toSeq).take(elementsPerBatch)

    def allSorted: Boolean = unplaced.size == 0

    def placingStr: String =
      placing.indices.toSeq.map(x => f"$x%02d").mkString(" | ") + "\n"
      placing.map {
        case Some(i) => f"$i%02d"
        case None => "  "
      }.mkString(" | ")

    def roachInfoStr: String =
      roaches.map { implicit x =>
        f"$x%02d ${left.size + right.size}%2d / ${rouchesTotal - 1} | ${left.mkString(" ", " ", " ")} <<< $x <<< ${right.mkString(" ", " ", " ")}"
      }.mkString("\n")

    def sorted: Seq[Roach] = roaches.sortBy(r => placing(r).getOrElse(-1))

  }

}
