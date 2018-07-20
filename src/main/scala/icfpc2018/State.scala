package icfpc2018

import scala.collection.SortedSet

object Extensions {
  implicit class IntToBase(val digits: Int) extends AnyVal {
    def base(b: Int) = Integer.parseInt(digits.toString, b).toByte
    def b = base(2)
    def o = base(8)
    def x = base(16)
  }
}

import Extensions._

sealed trait Harmonics {
  def flipped: Harmonics
}

case object Low extends Harmonics {
  val flipped = High
}

case object High extends Harmonics {
  val flipped = Low
}

sealed trait Voxel
case object Full extends Voxel
case object Void extends Voxel

case class State(
  energy: Long,
  harmonics: Harmonics,
  matrix: Matrix,
  bots: SortedSet[Bot],
  trace: List[Command])

case class Bot(
  bid: Int,
  pos: Coord,
  seeds: SortedSet[Int])

case class Coord(x: Int, y: Int, z: Int) {

  lazy val neighbors: List[Coord] = List(
    Coord(x - 1, y, z), Coord(x + 1, y, z),
    Coord(x, y - 1, z), Coord(x, y + 1, z),
    Coord(x, y, z - 1), Coord(x, y, z + 1))

  def +(lld: CoordinateDifference): Coord = lld.a match {
    case X => Coord(x + lld.len, y, z)
    case Y => Coord(x, y + lld.len, z)
    case Z => Coord(x, y, z + lld.len)
  }

  def +(nd: NCD): Coord =
    Coord(x + nd.dx, y + nd.dy, z + nd.dz)

  def rangeTo(cd: CoordinateDifference): List[Coord] =
    if (cd.len == 0) List(this)
    else (this + cd) :: rangeTo(cd match {
      case LLD(a, len) => LLD(a, len + (if (len > 0) -1 else 1))
      case SLD(a, len) => SLD(a, len + (if (len > 0) -1 else 1))
    })

  def manhattanDistanceTo(coord: Coord): Int =
    math.abs(coord.x - x) + math.abs(coord.y - y) + math.abs(coord.z - z)
}

object Coord {
  implicit val ordering: Ordering[Coord] = Ordering.by(o => (o.x, o.y, o.z))
}

sealed trait Command {
  def encoded: Vector[Byte]
}

case object Halt extends Command {
  val encoded = Vector(11111111.b)
}

case object Wait extends Command {
  val encoded = Vector(11111110.b)
}

case object Flip extends Command {
  val encoded = Vector(11111101.b)
}

case class SMove(lld: LLD) extends Command {
  val encoded = Vector(
    (100.b + (lld.a.encoded.toInt << 4)).toByte,
    lld.i.toByte)
}

case class LMove(sld1: SLD, sld2: SLD) extends Command {
  val encoded = Vector(
    (1100.b + (sld1.a.encoded.toInt << 4) + (sld2.a.encoded.toInt << 6)).toByte,
    ((sld2.i << 4) + sld1.i).toByte)
}

case class FusionP(nd: NCD) extends Command {
  val encoded = Vector((111.b + (nd.encoded << 3)).toByte)
}

case class FusionS(nd: NCD) extends Command {
  val encoded = Vector((110.b + (nd.encoded << 3)).toByte)
}

case class Fission(nd: NCD, m: Int) extends Command {
  val encoded = Vector((101.b + (nd.encoded << 3)).toByte, m.toByte)
}

case class Fill(nd: NCD) extends Command {
  val encoded = Vector((11.b + (nd.encoded << 3)).toByte)
}

trait CoordinateDifference {
  def a: Dir
  def len: Int
}

case class LLD(a: Dir, len: Int) extends CoordinateDifference {
  require(len >= -15 && len <= 15)

  def i = len + 15
}

case class SLD(a: Dir, len: Int) extends CoordinateDifference {
  require(len >= -5 && len <= 5)

  def i = len + 5
}

case class NCD(dx: Int, dy: Int, dz: Int) {
  def encoded: Int = (dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)

  require(math.abs(dx) <= 1)
  require(math.abs(dy) <= 1)
  require(math.abs(dz) <= 1)
  require {
    val mlen = math.abs(dx) + math.abs(dy) + math.abs(dz)
    mlen >= 1 && mlen <= 2
  }
}

sealed trait Dir {
  def encoded: Byte
}

case object X extends Dir {
  val encoded = 1.b
}

case object Y extends Dir {
  val encoded = 10.b
}

case object Z extends Dir {
  val encoded = 11.b
}
