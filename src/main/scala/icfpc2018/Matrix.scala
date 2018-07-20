package icfpc2018

import java.io.File
import java.nio.file.Files

import scala.annotation.tailrec

case class Matrix(dimension: Int, filledVoxels: Set[Coord] = Set.empty, groundedVoxels: Set[Coord] = Set.empty) {

  def validateCoord(coord: Coord): Boolean =
    coord.x >= 0 && coord.x < dimension &&
      coord.y >= 0 && coord.y < dimension &&
      coord.z >= 0 && coord.z < dimension

  def canFillCoord(coord: Coord): Boolean =
    coord.x >= 1 && coord.x < (dimension - 1) &&
      coord.y >= 0 && coord.y < dimension &&
      coord.z >= 1 && coord.z < (dimension - 1) &&
      !filledVoxels.contains(coord)

  def get(coord: Coord): Voxel = {
    require(validateCoord(coord), s"Invalid coordinate: $coord")
    if (filledVoxels.contains(coord)) Full else Void
  }

  def supported(coord: Coord): Boolean =
    coord.y == 0 || coord.neighbors.filter(validateCoord).exists(groundedVoxels)
  lazy val isGrounded: Boolean = filledVoxels.size == groundedVoxels.size

  private[this] def updatedGrounded(newCoord: Coord): Set[Coord] = {
    val (groundedInit, ungroundedInit): (Set[Coord], Set[Coord]) =
      if (supported(newCoord)) (groundedVoxels + newCoord, filledVoxels -- groundedVoxels)
      else (groundedVoxels, (filledVoxels -- groundedVoxels) + newCoord)
    @tailrec
    def aux(groundedAccum: Set[Coord], ungroundedAccum: Set[Coord]): Set[Coord] = {
      val canBeGrounded = ungroundedAccum.filter(_.neighbors.filter(validateCoord).exists(groundedAccum))
      if (canBeGrounded.isEmpty) groundedAccum
      else {
        aux(groundedAccum ++ canBeGrounded, ungroundedAccum -- canBeGrounded)
      }
    }
    aux(groundedInit, ungroundedInit)
  }

  def fill(coord: Coord): Matrix = {
    require(canFillCoord(coord), s"Can't fill coordinate: $coord")
    val newGrounded = if (supported(coord)) {
      if (isGrounded) groundedVoxels + coord
      else updatedGrounded(coord)
    } else groundedVoxels
    copy(filledVoxels = filledVoxels + coord, groundedVoxels = newGrounded)
  }

  def unsafeFill(coord: Coord): Matrix = {
    require(canFillCoord(coord), s"Can't fill coordinate: $coord")
    copy(filledVoxels = filledVoxels + coord, groundedVoxels = groundedVoxels + coord)
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
