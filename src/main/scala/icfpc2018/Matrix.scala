package icfpc2018

import java.io.File
import java.nio.file.Files

import scala.annotation.tailrec
import scala.collection.mutable

case class Matrix(
  dimension: Int,
  groundedVoxels: Set[Coord] = Set.empty,
  ungroundedVoxels: Set[Coord] = Set.empty) {

  lazy val voxels: Set[Coord] = groundedVoxels ++ ungroundedVoxels

  def validateCoord(coord: Coord): Boolean =
    coord.x >= 0 && coord.x < dimension &&
      coord.y >= 0 && coord.y < dimension &&
      coord.z >= 0 && coord.z < dimension

  def canFillCoord(coord: Coord): Boolean =
    coord.x >= 1 && coord.x < (dimension - 1) &&
      coord.y >= 0 && coord.y < dimension &&
      coord.z >= 1 && coord.z < (dimension - 1) &&
      !voxels.contains(coord)

  def get(coord: Coord): Voxel = {
    require(validateCoord(coord), s"Invalid coordinate: $coord")
    if (voxels.contains(coord)) Full else Void
  }

  def supported(coord: Coord): Boolean =
    coord.y == 0 || coord.neighbors.filter(validateCoord).exists(groundedVoxels)
  lazy val isGrounded: Boolean = ungroundedVoxels.isEmpty

  private[this] def updatedGrounded(newCoord: Coord): (Set[Coord], Set[Coord]) = {
    val (groundedInit, ungroundedInit): (Set[Coord], Set[Coord]) =
      if (supported(newCoord)) (groundedVoxels + newCoord, ungroundedVoxels)
      else (groundedVoxels, ungroundedVoxels + newCoord)
    @tailrec
    def aux(groundedAccum: Set[Coord], ungroundedAccum: Set[Coord]): (Set[Coord], Set[Coord]) = {
      val canBeGrounded = ungroundedAccum.filter(_.neighbors.filter(validateCoord).exists(groundedAccum))
      if (canBeGrounded.isEmpty) (groundedAccum, ungroundedAccum)
      else {
        aux(groundedAccum ++ canBeGrounded, ungroundedAccum -- canBeGrounded)
      }
    }
    aux(groundedInit, ungroundedInit)
  }

  def fill(coord: Coord): Matrix = {
    require(canFillCoord(coord), s"Can't fill coordinate: $coord")
    val (newGrounded, newUngrounded) = if (supported(coord)) {
      if (isGrounded) (groundedVoxels + coord, ungroundedVoxels)
      else updatedGrounded(coord)
    } else (groundedVoxels, ungroundedVoxels + coord)
    copy(groundedVoxels = newGrounded, ungroundedVoxels = newUngrounded)
  }

  def unsafeFill(coord: Coord): Matrix = {
    require(canFillCoord(coord), s"Can't fill coordinate: $coord")
    copy(groundedVoxels = groundedVoxels + coord)
  }

  /**
   * Returns a valid list of movement commands (SMove or LMove) that start from a given coord.
   */
  def validMoves(from: Coord): List[Command] = {
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
          if (validDir(dir)(math.signum(i)) && from.rangeTo(lld).forall(x => validateCoord(x) && get(x) == Void)) {
            val nextCoord = from + lld
            valids += SMove(lld)
            if (math.abs(i) <= 5) {
              dirs.foreach { otherDir =>
                if (otherDir != dir) {
                  (-5 to 5).foreach { j =>
                    if (j != 0) {
                      val sld = SLD(otherDir, j)
                      if (nextCoord.rangeTo(sld).forall(x => validateCoord(x) && get(x) == Void)) {
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

  def path(from: Coord, to: Coord): List[Command] = {
    val stepCost = dimension * dimension * dimension

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

    return path.reverse.toList
  }
}

object Matrix {
  def fromMdl(mdlFile: File): Matrix = {
    val bytes: Array[Byte] = Files.readAllBytes(mdlFile.toPath)
    val matrix = Matrix(0xFF & bytes(0).asInstanceOf[Int])
    var z = 0
    var x = 0
    var y = 0
    bytes.drop(1).foldLeft(matrix) {
      case (m, b) =>
        (0 until 8).foldLeft(m) {
          case (m, i) =>
            val nextM = if ((b & (1 << i)) != 0)
              m.unsafeFill(Coord(x, y, z))
            else
              m

            z += 1
            if (z >= matrix.dimension) {
              z = 0
              y += 1
            }

            if (y >= matrix.dimension) {
              y = 0
              x += 1
            }

            nextM
        }
    }
  }
}
