package icfpc2018.solver.pathing

import scala.collection.mutable

import icfpc2018._

class AStarPathFinder(model: Matrix) {
  private[this] def validMoves(from: Coord): List[Command] = {
    val valids = mutable.ListBuffer[Command]()
    val dirs = List(X, Y, Z)
    val validDir = mutable.Map(
      X -> mutable.Map(-1 -> true, 1 -> true),
      Y -> mutable.Map(-1 -> true, 1 -> true),
      Z -> mutable.Map(-1 -> true, 1 -> true))

    (-15 to 15).foreach { i =>
      if (i != 0) {
        dirs.foreach { dir =>
          val lld = LLD(dir, i)
          if (validDir(dir)(math.signum(i)) && from.rangeToIterator(lld).forall(x => model.validateCoord(x) && model.get(x) == Void)) {
            val nextCoord = from + lld
            valids += SMove(lld)
            if (math.abs(i) <= 5) {
              dirs.foreach { otherDir =>
                if (otherDir != dir) {
                  (-5 to 5).foreach { j =>
                    if (j != 0) {
                      val sld = SLD(otherDir, j)
                      if (nextCoord.rangeToIterator(sld).forall(x => model.validateCoord(x) && model.get(x) == Void)) {
                        valids += LMove(SLD(dir, i), sld)
                      }
                    }
                  }
                }
              }
            }
          } else {
            validDir(dir)(math.signum(i)) = false
          }
        }
      }
    }

    valids.toList
  }

  def findPath(from: Coord, to: Coord): List[Command] = {
    val stepCost = model.dimension * model.dimension * model.dimension

    val visited = mutable.Map[Coord, Long]()
    val prev = mutable.Map[Coord, (Coord, Command)]()

    val pq = mutable.PriorityQueue[(Long, Coord)]()
    pq.enqueue((0, from))
    visited(from) = 0

    var keepGoing = true

    while (keepGoing && pq.nonEmpty) {
      val (nlen, curr) = pq.dequeue()
      val len = -nlen
      if (curr == to)
        keepGoing = false
      if (visited(curr) >= len) {
        validMoves(curr).foreach {
          case SMove(lld) =>
            val nextCoord = curr + lld
            val cost = len + (2 * math.abs(lld.len)) + stepCost + curr.manhattanDistanceTo(nextCoord)
            if (visited.get(nextCoord).isEmpty || visited(nextCoord) > cost) {
              visited(nextCoord) = cost
              prev(nextCoord) = (curr, SMove(lld))
              pq.enqueue((-cost, nextCoord))
            }
          case LMove(sld1, sld2) =>
            val nextCoord = curr + sld1 + sld2
            val cost = len + (2 * (math.abs(sld1.len) + 2 + math.abs(sld2.len))) + stepCost + curr.manhattanDistanceTo(nextCoord)
            if (visited.get(nextCoord).isEmpty || visited(nextCoord) > cost) {
              visited(nextCoord) = cost
              prev(nextCoord) = (curr, LMove(sld1, sld2))
              pq.enqueue((-cost, nextCoord))
            }
          case _ => // do nothing
        }
      }
    }

    var f = to
    val path = mutable.ListBuffer[Command]()
    while (f != from) {
      path += prev(f)._2
      f = prev(f)._1
    }

    path.reverse.toList
  }
}
