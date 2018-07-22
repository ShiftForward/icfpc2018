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
      smoves
      /*smoves ++ (if (math.abs(len) <= 5) {
        for {
          sld2 <- (for {
            dir2 <- Dir.all.filter(_ != dir)
          } yield (validShortMovesAux(next, dir2, 1, 1) ++ validShortMovesAux(next, dir2, -1, -1))).flatten.map(_.asInstanceOf[SMove])
        } yield LMove(SLD(lld.a, lld.len), SLD(sld2.lld.a, sld2.lld.len))
      } else {
        Nil
      })*/
    }
  }

  private[this] def validMoves(from: Coord): List[Command] =
    Dir.all.flatMap { dir => validMovesAux(from, dir, 1, 1) ++ validMovesAux(from, dir, -1, -1) }

  private[this] def yFindPathAux(from: Coord, to: Coord, cache: mutable.Map[Coord, List[Command]]): List[Command] =
    cache.getOrElseUpdate(from, {
      if (from == to)
        Nil
      else if (from.z == to.z) {
        val len = to.x - from.x
        val lld = if (math.abs(len) > 15)
          LLD(X, math.signum(len) * 15)
        else
          LLD(X, len)
        SMove(lld) :: yFindPathAux(from + lld, to, cache)
      } else if (from.x == to.x) {
        val len = to.z - from.z
        val lld = if (math.abs(len) > 15)
          LLD(Z, math.signum(len) * 15)
        else
          LLD(Z, len)
        SMove(lld) :: yFindPathAux(from + lld, to, cache)
      } else {
        val diffX = to.x - from.x
        val diffZ = to.z - from.z
        val dx = math.signum(diffX)
        val dz = math.signum(diffZ)
        val maxX = if (dx > 0) math.min(5, diffX) else math.max(-5, diffX)
        val maxZ = if (dz > 0) math.min(5, diffZ) else math.max(-5, diffZ)

        var best: List[Command] = Nil
        var command: Command = LMove(SLD(X, 0), SLD(X, 0))

        (dx to maxX by dx).foreach { x =>
          val sld1 = SLD(X, x)
          (dz to maxZ by dz).foreach { z =>
            val sld2 = SLD(Z, z)
            val next = yFindPathAux(from + sld1 + sld2, to, cache)
            if (best.isEmpty || next.length < best.length) {
              best = next
              command = LMove(sld1, sld2)
            }
          }
        }

        command :: best
      }
    })

  def yFindPath(from: Coord, to: Coord): List[Command] = yFindPathAux(from, to, mutable.Map()).reverse

  def findPath(from: Coord, to: Coord): List[Command] = {
    if (from.y == to.y && model.countY(from.y) == 0)
      return yFindPath(from, to)

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
