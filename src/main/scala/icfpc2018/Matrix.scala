package icfpc2018

import java.io.File
import java.nio.file.Files

import scala.annotation.tailrec
import scala.collection.{ BitSet, mutable }

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
  lazy val centerOfMass: Coord = {
    val (x, y, z) = voxels.foldLeft((0, 0, 0)) { case ((sumX, sumY, sumZ), c) => (sumX + c.x, sumY + c.y, sumZ + c.z) }
    Coord(x / voxels.size, y / voxels.size, z / voxels.size)
  }

  lazy val sumOfFilled: Map[Coord, Int] = {
    val map = mutable.Map.empty[Coord, Int]

    def value(x: Int, y: Int, z: Int): Int = {
      val c = Coord(x, y, z)
      if (!validateCoord(c)) 0
      else map.get(c).get
    }

    (0 until dimension).map { x =>
      (0 until dimension).map { y =>
        (0 until dimension).map { z =>
          val v = if (voxels.contains(Coord(x, y, z))) 1 else 0
          val vx = value(x - 1, y, z)
          val vy = value(x, y - 1, z)
          val vz = value(x, y, z - 1)
          val vxyz = value(x - 1, y - 1, z - 1)
          val vxy = value(x - 1, y - 1, z)
          val vxz = value(x - 1, y, z - 1)
          val vyz = value(x, y - 1, z - 1)

          val sum = v + vx + vy + vz + vxyz
          val minus = vxy + vxz + vyz

          map.put(Coord(x, y, z), (v + vx + vy + vz + vxyz - vxy - vxz - vyz))
        }
      }
    }
    map.toMap
  }

  /**
   *
   * @param c1 one corner of the "cube"
   * @param c2 the diagonal corner of the "cube"
   * @return Option[Voxel] -> Some if full volume has same value
   */
  def isVoidOrFull(c1: Coord, c2: Coord): Option[Voxel] = {
    require(validateCoord(c1) && validateCoord(c2), "c1 and c2 must be valid")
    def value(x: Int, y: Int, z: Int): Int = {
      val c = Coord(x, y, z)
      if (!validateCoord(c)) 0
      else sumOfFilled.get(c).get
    }

    val xMax = math.max(c1.x, c2.x)
    val xMin = math.min(c1.x, c2.x) - 1
    val yMax = math.max(c1.y, c2.y)
    val yMin = math.min(c1.y, c2.y) - 1
    val zMax = math.max(c1.z, c2.z)
    val zMin = math.min(c1.z, c2.z) - 1

    val v1 = value(xMin, yMax, zMax)
    val v2 = value(xMax, yMax, zMin)
    val v3 = value(xMax, yMin, zMax)
    val v4 = value(xMax, yMin, zMin)
    val v5 = value(xMin, yMin, zMax)
    val vmin = value(xMin, yMin, zMin)
    val vmax = value(xMax, yMax, zMax)

    val sum = vmax - v1 - v2 - v3 + v4 + v5 + vmin
    val volume = (xMax - xMin) * (yMax - yMin) * (zMax - zMin)

    if (sum == 0) Some(Void)
    else if (sum == volume) Some(Full)
    else None
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
