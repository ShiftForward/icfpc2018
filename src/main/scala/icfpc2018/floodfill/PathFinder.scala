package icfpc2018.floodfill

import scala.collection.mutable

import icfpc2018._

class PathFinder(to: Coord, stMatrix: Matrix) {
  private val visited: mutable.Set[Coord] = mutable.Set()

  def findPath(initialCoord: Coord): Option[List[Command]] = {
    val q = mutable.Queue[(Coord, List[Command])]()
    q.enqueue((initialCoord, Nil))
    visited += initialCoord

    while (q.nonEmpty) {
      val (coord, acc) = q.dequeue()
      if (coord == to) return Some(acc)

      for (a <- List(X, Y, Z)) {
        for (len <- List(1, -1)) {
          val lld = LLD(a, len)
          val newCoord = coord + lld

          if (stMatrix.validateCoord(newCoord) && stMatrix.get(newCoord) == Void && !visited(newCoord)) {
            q.enqueue((newCoord, SMove(lld) :: acc))
            visited += newCoord
          }
        }
      }
    }
    None
  }
}

object PathFinder {

  // TL;DR the first command must be a SMove with len = 1.
  // TODO: we don't yet support anything other than unit steps here due to FloodFill
  def findPath(from: Coord, to: Coord, stMatrix: Matrix): Option[List[Command]] = {
    val pf = new PathFinder(to, stMatrix)
    pf.findPath(from).map(_.reverse)
  }
}
