package icfpc2018.solver.pathing

import scala.collection.mutable

import icfpc2018._

class AStarPathFinder(model: Matrix, botPositions: Set[Coord]) {
  /*private[this] def validShortMovesAux(from: Coord, dir: Dir, len: Int, dl: Int): List[Command] =
    if (math.abs(len) > 5 || !model.validAndNotFilled(from + LLD(dir, len))) {
      Nil
    } else {
      SMove(LLD(dir, len)) :: validShortMovesAux(from, dir, len + dl, dl)
    }*/

  private[this] def validMovesAux(from: Coord, dir: Dir, len: Int, dl: Int): List[Command] = {
    if (math.abs(len) > 15 || !model.validAndNotFilled(from + LLD(dir, len)) || botPositions.contains(from + LLD(dir, len))) {
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

  private[this] def yFindPathAux(from: Coord, to: Coord): List[Command] =
    AStarPathFinder.yPathCache.getOrElseUpdate(Coord(to.x - from.x, to.y - from.y, to.z - from.z), {
      if (from == to)
        Nil
      else if (from.z == to.z) {
        val len = to.x - from.x
        val lld = if (math.abs(len) > 15)
          LLD(X, math.signum(len) * 15)
        else
          LLD(X, len)
        SMove(lld) :: yFindPathAux(from + lld, to)
      } else if (from.x == to.x) {
        val len = to.z - from.z
        val lld = if (math.abs(len) > 15)
          LLD(Z, math.signum(len) * 15)
        else
          LLD(Z, len)
        SMove(lld) :: yFindPathAux(from + lld, to)
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
            val next = yFindPathAux(from + sld1 + sld2, to)
            if (best.isEmpty || next.length < best.length) {
              best = next
              command = LMove(sld1, sld2)
            }
          }
        }

        command :: best
      }
    })

  def yFindPath(from: Coord, to: Coord): List[Command] = yFindPathAux(from, to).reverse

  def findPath(from: Coord, to: Coord): List[Command] = {
    if (from.y == to.y) {
      val dx = if (to.x - from.x == 0) 1 else math.signum(to.x - from.x)
      val dz = if (to.z - from.z == 0) 1 else math.signum(to.z - from.z)
      if ((from.x to to.x by dx).forall(x => (from.z to to.z by dz).forall(
        z => model.get(Coord(x, from.y, z)) == Void && !botPositions.contains(Coord(x, from.y, z)))))
        return yFindPath(from, to)
    }

    val stepCost = model.dimension * model.dimension * model.dimension

    val visited = Array.fill(model.dimension, model.dimension, model.dimension)(-1l)
    val prevCoord = Array.ofDim[Coord](model.dimension, model.dimension, model.dimension)
    val prevCommand = Array.ofDim[Command](model.dimension, model.dimension, model.dimension)

    val pq = mutable.PriorityQueue[(Long, Coord)]()
    pq.enqueue((0, from))
    visited(from.x)(from.y)(from.z) = 0

    var keepGoing = true

    while (keepGoing && pq.nonEmpty) {
      val (nlen, curr) = pq.dequeue()
      val len = -nlen
      if (curr == to)
        keepGoing = false
      if (keepGoing && visited(curr.x)(curr.y)(curr.z) >= len) {
        validMoves(curr).foreach {
          case SMove(lld) =>
            val nextCoord = curr + lld
            val cost = len + (2 * math.abs(lld.len)) + stepCost + curr.manhattanDistanceTo(nextCoord)
            val currV = visited(nextCoord.x)(nextCoord.y)(nextCoord.z)
            if (currV == -1 || currV > cost) {
              visited(nextCoord.x)(nextCoord.y)(nextCoord.z) = cost
              prevCoord(nextCoord.x)(nextCoord.y)(nextCoord.z) = curr
              prevCommand(nextCoord.x)(nextCoord.y)(nextCoord.z) = SMove(lld)
              pq.enqueue((-cost, nextCoord))
            }
          case LMove(sld1, sld2) =>
            val nextCoord = curr + sld1 + sld2
            val cost = len + (2 * (math.abs(sld1.len) + 2 + math.abs(sld2.len))) + stepCost + curr.manhattanDistanceTo(nextCoord)
            val currV = visited(nextCoord.x)(nextCoord.y)(nextCoord.z)
            if (currV == -1 || currV > cost) {
              visited(nextCoord.x)(nextCoord.y)(nextCoord.z) = cost
              prevCoord(nextCoord.x)(nextCoord.y)(nextCoord.z) = curr
              prevCommand(nextCoord.x)(nextCoord.y)(nextCoord.z) = LMove(sld1, sld2)
              pq.enqueue((-cost, nextCoord))
            }
          case _ => // do nothing
        }
      }
    }

    var f = to
    val path = mutable.ListBuffer[Command]()
    while (f != from) {
      val pCoord = prevCoord(f.x)(f.y)(f.z)
      if (pCoord == null)
        return Nil
      path += prevCommand(f.x)(f.y)(f.z)
      f = pCoord
    }

    path.reverse.toList
  }
}

object AStarPathFinder {
  val yPathCache = mutable.Map[Coord, List[Command]]()
}
