package icfpc2018

import java.io.File
import java.nio.file.Files

import scala.annotation.tailrec
import scala.collection.BitSet

import icfpc2018.Matrix.CoordSet

case class Matrix(
  dimension: Int,
  groundedVoxels: CoordSet = CoordSet(),
  ungroundedVoxels: CoordSet = CoordSet(),
  emptyX: Map[Int, Boolean] = Map[Int, Boolean]().withDefaultValue(true),
  emptyY: Map[Int, Boolean] = Map[Int, Boolean]().withDefaultValue(true),
  emptyZ: Map[Int, Boolean] = Map[Int, Boolean]().withDefaultValue(true)) {
  require(dimension <= 250, "Max matrix dimension is 250")

  lazy val voxels: Set[Coord] = groundedVoxels ++ ungroundedVoxels

  def validateCoord(coord: Coord): Boolean =
    coord.x >= 0 && coord.x < dimension &&
      coord.y >= 0 && coord.y < dimension &&
      coord.z >= 0 && coord.z < dimension

  def validAndNotFilled(coord: Coord): Boolean =
    validateCoord(coord) && !voxels.contains(coord)

  def canFillCoord(coord: Coord): Boolean =
    coord.x >= 1 && coord.x < (dimension - 1) &&
      coord.y >= 0 && coord.y < (dimension - 1) &&
      coord.z >= 1 && coord.z < (dimension - 1) &&
      !voxels.contains(coord)

  def get(coord: Coord): Voxel = {
    require(validateCoord(coord), s"Invalid coordinate: $coord")
    if (voxels.contains(coord)) Full else Void
  }

  def supported(coord: Coord): Boolean =
    coord.y == 0 || coord.neighbors.filter(validateCoord).exists(groundedVoxels)
  lazy val isGrounded: Boolean = ungroundedVoxels.isEmpty

  private[this] def updatedGrounded(newCoord: Coord): (CoordSet, CoordSet) = {
    val (groundedInit, ungroundedInit): (CoordSet, CoordSet) =
      if (supported(newCoord)) (groundedVoxels + newCoord, ungroundedVoxels)
      else (groundedVoxels, ungroundedVoxels + newCoord)
    @tailrec
    def aux(groundedAccum: CoordSet, ungroundedAccum: CoordSet): (CoordSet, CoordSet) = {
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
    copy(
      groundedVoxels = newGrounded,
      ungroundedVoxels = newUngrounded,
      emptyX = emptyX.updated(coord.x, false),
      emptyY = emptyY.updated(coord.y, false),
      emptyZ = emptyZ.updated(coord.z, false))
  }

  def unsafeFill(coord: Coord): Matrix = {
    require(canFillCoord(coord), s"Can't fill coordinate: $coord")
    copy(
      groundedVoxels = groundedVoxels + coord,
      emptyX = emptyX.updated(coord.x, false),
      emptyY = emptyY.updated(coord.y, false),
      emptyZ = emptyZ.updated(coord.z, false))
  }
}

object Matrix {

  case class CoordSet(innerBuffer: BitSet = BitSet.empty) extends Set[Coord] {
    @inline
    final def fromBinary(binary: Int): Coord = Coord((binary >> 16) & 0xFF, (binary >> 8) & 0xFF, binary & 0xFF)

    override def contains(elem: Coord): Boolean = innerBuffer(elem.toBinary)
    override def +(elem: Coord): CoordSet = copy(innerBuffer + elem.toBinary)
    def ++(that: CoordSet): CoordSet = CoordSet(this.innerBuffer | that.innerBuffer)
    override def -(elem: Coord): CoordSet = copy(innerBuffer - elem.toBinary)
    def --(that: CoordSet): CoordSet = CoordSet(this.innerBuffer &~ that.innerBuffer)
    override def filter(p: Coord => Boolean): CoordSet = CoordSet(
      innerBuffer.filter(x => p(fromBinary(x))))
    override def iterator: Iterator[Coord] = innerBuffer.iterator.map(fromBinary)
  }

  def fromMdl(mdlFile: File): Matrix = {
    val bytes: Array[Byte] = Files.readAllBytes(mdlFile.toPath)
    val matrix = Matrix(0xFF & bytes(0).asInstanceOf[Int])
    println(s"Loading $mdlFile model file with dimension: ${matrix.dimension}")
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
