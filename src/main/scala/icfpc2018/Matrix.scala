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
  countX: Map[Int, Int] = Map[Int, Int]().withDefaultValue(0),
  countY: Map[Int, Int] = Map[Int, Int]().withDefaultValue(0),
  countZ: Map[Int, Int] = Map[Int, Int]().withDefaultValue(0)) {
  require(dimension <= 250, "Max matrix dimension is 250")

  lazy val voxels: Set[Coord] = groundedVoxels ++ ungroundedVoxels
  lazy val centerOfMass: Coord = {
    val (x, y, z) = voxels.foldLeft((0, 0, 0)) { case ((sumX, sumY, sumZ), c) => (sumX + c.x, sumY + c.y, sumZ + c.z) }
    Coord(x / voxels.size, y / voxels.size, z / voxels.size)
  }

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

  def canVoidCoord(coord: Coord): Boolean =
    coord.x >= 1 && coord.x < (dimension - 1) &&
      coord.y >= 0 && coord.y < (dimension - 1) &&
      coord.z >= 1 && coord.z < (dimension - 1) &&
      voxels.contains(coord)

  def get(coord: Coord): Voxel = {
    require(validateCoord(coord), s"Invalid coordinate: $coord")
    if (voxels.contains(coord)) Full else Void
  }

  def supported(coord: Coord): Boolean =
    coord.y == 0 || coord.neighbors.filter(validateCoord).exists(groundedVoxels)
  lazy val isGrounded: Boolean = ungroundedVoxels.isEmpty

  private[this] def updatedGrounded(grounded: CoordSet, unknown: CoordSet, ungrounded: CoordSet): (CoordSet, CoordSet) = {
    @tailrec
    def aux(groundedAccum: CoordSet, unknownAccum: CoordSet): (CoordSet, CoordSet) = {
      val canBeGrounded = unknownAccum.filter(_.neighbors.filter(validateCoord).exists(groundedAccum))
      if (canBeGrounded.isEmpty) (groundedAccum, unknownAccum)
      else {
        aux(groundedAccum ++ canBeGrounded, unknownAccum -- canBeGrounded)
      }
    }
    if (unknown.isEmpty) (grounded, ungrounded)
    else {
      val (finalGrounded, updatedUngrounded) = aux(grounded, unknown)
      (finalGrounded, ungrounded ++ updatedUngrounded)
    }
  }

  private[this] def updatedGroundedFill(newCoord: Coord): (CoordSet, CoordSet) = {
    if (supported(newCoord))
      updatedGrounded(groundedVoxels + newCoord, ungroundedVoxels, CoordSet())
    else updatedGrounded(groundedVoxels, ungroundedVoxels + newCoord, CoordSet())
  }

  private[this] def updateGroundedVoid(newCoord: Coord): (CoordSet, CoordSet) = {
    if (supported(newCoord)) {
      val (floored, lifted) = (groundedVoxels - newCoord).partition(_.y == 0)
      updatedGrounded(floored, lifted, ungroundedVoxels)
    } else updatedGrounded(groundedVoxels, CoordSet(), ungroundedVoxels - newCoord)
  }

  def fill(coord: Coord): Matrix = {
    require(canFillCoord(coord), s"Can't fill coordinate: $coord")
    val (newGrounded, newUngrounded) = if (supported(coord)) {
      if (isGrounded) (groundedVoxels + coord, ungroundedVoxels)
      else updatedGroundedFill(coord)
    } else (groundedVoxels, ungroundedVoxels + coord)
    copy(
      groundedVoxels = newGrounded,
      ungroundedVoxels = newUngrounded,
      countX = countX.updated(coord.x, countX(coord.x) + 1),
      countY = countY.updated(coord.y, countY(coord.y) + 1),
      countZ = countZ.updated(coord.z, countZ(coord.z) + 1))
  }

  def void(coord: Coord): Matrix = {
    require(canVoidCoord(coord), s"Can't void coordinate: $coord")
    val (newGrounded, newUngrounded) = if (supported(coord)) {
      val degree = coord.neighbors.count(c => c.y < 0 || voxels.contains(c))
      if (degree == 1) (groundedVoxels - coord, ungroundedVoxels)
      else updateGroundedVoid(coord)
    } else (groundedVoxels, ungroundedVoxels - coord)
    copy(
      groundedVoxels = newGrounded,
      ungroundedVoxels = newUngrounded) // FIXME: Update empty
  }

  def unsafeFill(coord: Coord): Matrix = {
    require(canFillCoord(coord), s"Can't fill coordinate: $coord")
    copy(
      groundedVoxels = groundedVoxels + coord,
      countX = countX.updated(coord.x, countX(coord.x) + 1),
      countY = countY.updated(coord.y, countY(coord.y) + 1),
      countZ = countZ.updated(coord.z, countZ(coord.z) + 1))
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
    override def partition(p: Coord => Boolean): (CoordSet, CoordSet) = {
      val (t, f) = innerBuffer.partition(x => p(fromBinary(x)))
      (CoordSet(t), CoordSet(f))
    }
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
