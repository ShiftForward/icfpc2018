package icfpc2018

import scala.collection.SortedSet
import scala.util.Try

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

  def rangeToIterator(cd: CoordinateDifference): Iterator[Coord] = {
    def nextCd(cd: CoordinateDifference) = cd match {
      case LLD(a, len) => LLD(a, len + (if (len > 0) -1 else 1))
      case SLD(a, len) => SLD(a, len + (if (len > 0) -1 else 1))
    }
    Iterator.iterate(cd)(nextCd).takeWhile(_.len != 0).map(cd => this + cd) ++ Iterator(this)
  }

  def rangeTo(cd: CoordinateDifference): List[Coord] = rangeToIterator(cd).toList

  def manhattanDistanceTo(coord: Coord): Int =
    math.abs(coord.x - x) + math.abs(coord.y - y) + math.abs(coord.z - z)

  lazy val toBinary: Int = (x << 16) | (y << 8) | z
}

object Coord {
  implicit val ordering: Ordering[Coord] = Ordering.by(o => (o.x, o.y, o.z))
}

sealed trait Command {
  def encoded: Vector[Byte]
}

case object Halt extends Command {
  final val encoded = Vector(11111111.b)
}

case object Wait extends Command {
  final val encoded = Vector(11111110.b)
}

case object Flip extends Command {
  final val encoded = Vector(11111101.b)
}

case class SMove(lld: LLD) extends Command {
  final val header = 100.b
  lazy val encoded = Vector(
    (header + (lld.a.encoded.toInt << 4)).toByte,
    lld.i.toByte)
}

case class LMove(sld1: SLD, sld2: SLD) extends Command {
  final val header = 1100.b
  lazy val encoded = Vector(
    (header + (sld1.a.encoded.toInt << 4) + (sld2.a.encoded.toInt << 6)).toByte,
    ((sld2.i << 4) + sld1.i).toByte)
}

case class FusionP(nd: NCD) extends Command {
  final val header = 111.b
  lazy val encoded = Vector((header + (nd.encoded << 3)).toByte)
}

case class FusionS(nd: NCD) extends Command {
  final val header = 110.b
  lazy val encoded = Vector((header + (nd.encoded << 3)).toByte)
}

case class Fission(nd: NCD, m: Int) extends Command {
  final val header = 101.b
  lazy val encoded = Vector((header + (nd.encoded << 3)).toByte, m.toByte)
}

case class Fill(nd: NCD) extends Command {
  final val header = 11.b
  lazy val encoded = Vector((header + (nd.encoded << 3)).toByte)
}

trait CoordinateDifference {
  def a: Dir
  def len: Int
}

case class LLD(a: Dir, len: Int) extends CoordinateDifference {
  require(len >= -15 && len <= 15)

  def i = len + 15
}

case object LLD {
  def forMove(c1: Coord, c2: Coord): Option[LLD] = (c1, c2) match {
    case (Coord(x1, y1, z1), Coord(x2, y2, z2)) if math.abs(x1 - x2) <= 15 && y1 == y2 && z1 == z2 => Some(LLD(X, x2 - x1))
    case (Coord(x1, y1, z1), Coord(x2, y2, z2)) if math.abs(y1 - y2) <= 15 && x1 == x2 && z1 == z2 => Some(LLD(Y, y2 - y1))
    case (Coord(x1, y1, z1), Coord(x2, y2, z2)) if math.abs(z1 - z2) <= 15 && x1 == x2 && y1 == y2 => Some(LLD(Z, z2 - z1))
    case _ => None
  }
}

case class SLD(a: Dir, len: Int) extends CoordinateDifference {
  require(len >= -5 && len <= 5)

  def i = len + 5
}

case class NCD(dx: Int, dy: Int, dz: Int) {
  lazy val encoded: Int = (dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)

  require(math.abs(dx) <= 1)
  require(math.abs(dy) <= 1)
  require(math.abs(dz) <= 1)
  require {
    val mlen = math.abs(dx) + math.abs(dy) + math.abs(dz)
    mlen >= 1 && mlen <= 2
  }
}

case object NCD {
  def forMove(c1: Coord, c2: Coord): Option[NCD] = {
    Try(NCD(c2.x - c1.x, c2.y - c1.y, c2.z - c1.z)).toOption
  }
}

sealed trait Dir {
  def encoded: Byte
}

case object X extends Dir {
  final val encoded = 1.b
}

case object Y extends Dir {
  final val encoded = 10.b
}

case object Z extends Dir {
  final val encoded = 11.b
}
