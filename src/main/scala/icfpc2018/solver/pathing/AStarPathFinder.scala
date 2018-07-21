package icfpc2018.solver.pathing

import scala.collection.mutable

import icfpc2018._

class AStarPathFinder(model: Matrix) {
  private[this] def validShortMovesAux(from: Coord, dir: Dir, len: Int, dl: Int): List[Command] =
    if (math.abs(len) > 5 || !model.validAndNotFilled(from + LLD(dir, len))) {
      Nil
    } else {
      SMove(LLD(dir, len)) :: validShortMovesAux(from, dir, len + dl, dl)
    }

  private[this] def validMovesAux(from: Coord, dir: Dir, len: Int, dl: Int): List[Command] = {
    if (math.abs(len) > 15 || !model.validAndNotFilled(from + LLD(dir, len))) {
      Nil
    } else {
      val lld = LLD(dir, len)
      val next = from + lld
      val smoves: List[Command] = SMove(lld) :: validMovesAux(from, dir, len + dl, dl)
      smoves ++ (if (math.abs(len) <= 5) {
        for {
          sld2 <- (for {
            dir2 <- Dir.all.filter(_ != dir)
          } yield (validShortMovesAux(next, dir2, 1, 1) ++ validShortMovesAux(next, dir2, -1, -1))).flatten.map(_.asInstanceOf[SMove])
        } yield LMove(SLD(lld.a, lld.len), SLD(sld2.lld.a, sld2.lld.len))
      } else {
        Nil
      })
    }
  }

  private[this] def validMoves(from: Coord): List[Command] =
    Dir.all.flatMap { dir => validMovesAux(from, dir, 1, 1) ++ validMovesAux(from, dir, -1, -1) }

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
