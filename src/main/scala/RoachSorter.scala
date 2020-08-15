import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

object RoachSorter {
  type Roach = Int

  class RoachSorter(val rouchesTotal: Int = 25, val elementsPerBatch: Int = 5) {

    def validateState() = {
      for (i <- 0 until rouchesTotal) {
        given Roach = i
        assert((left & right) isEmpty)
        assert(!(left.contains(i)))
        assert(!(right.contains(i)))
        assert((left & right).size < rouchesTotal)

        val inLeft:Set[Roach] = roaches.filter(left(_).contains(i)) .toSet
        val inRight:Set[Roach] = roaches.filter(right(_).contains(i)).toSet
        if(left != inRight) println(s"$i left : $left inRight: $inRight diff l\\r ${left &~ inRight} r\\l ${inRight &~ left}")
        if(right != inLeft) println(s"$i  right : $right inLeft: $inLeft")
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

    def unplaced: Set[Roach] = {
      val ret: mutable.Buffer[Roach] = mutable.Buffer()
      var candidats: Seq[Roach] = roachesSet.filter(placeUnknown).toSeq
      while (ret.size < elementsPerBatch && candidats.nonEmpty) {
        val sorted: Seq[Roach] = if (ret.isEmpty)
          candidats.sortBy(implicit x => left.size + right.size)
        else {
//          candidats.sortBy(x => (ret.map( r => if(left(r).contains(x) || right(r).contains(x)) 1 else 0 ).sum, left(x).size + right(x).size))
          candidats.sortBy(x => ret.map( r => if(left(r).contains(x) || right(r).contains(x)) 1 else 0 ).sum)//if (ret.map(left(_)).contains(x) || ret.map(right(_)).contains(x)) 1 else 0)          
        }
//        println(candidats)
        candidats = sorted.tail
        ret += sorted.head
      }
      ret.toSet
    }

    def place(implicit roach: Roach): Unit = placing(roach) = Some(left.size)

    def processComparasionResults(res: IndexedSeq[Roach]): Unit =
      for (
        i <- 0 until res.size;
        j <- (i + 1) until res.size
      ) {
        val leftmost = res(i)
        val rightmost = res(j)
        r(leftmost) += rightmost
        l(rightmost) += leftmost
        r(leftmost) ++= right(rightmost)
        for(rn <- right(rightmost))l(rn) += leftmost
        l(rightmost) ++= left(leftmost)
        for(ln <- left(leftmost))r(ln) += rightmost

      }
      unplaced.filter(isAllLRKnown).foreach(place)

    def roachToSort: Seq[Roach] = new Random().shuffle(unplaced.toSeq).take(elementsPerBatch)

    def allSorted: Boolean = unplaced.size == 0

    def placingStr: String =
      placing.indices.toSeq.map(x => f"$x%02d").mkString(" | ") + "\n"
      placing.map {
        case Some(i) => f"$i%02d"
        case None => "  "
      }.mkString(" | ")

    def roachInfoStr: String =
      roaches.map { implicit x =>
        f"$x%02d | ${left.mkString(" ", " ", " ")} <<< $x <<< ${right.mkString(" ", " ", " ")}"
      }.mkString("\n")

    def sorted:Seq[Roach] = roaches.sortBy(r => placing(r).getOrElse(-1))

  }

}

