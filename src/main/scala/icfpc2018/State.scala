package icfpc2018

object Extensions {
  implicit class IntToBase(val digits: Int) extends AnyVal {
    def base(b: Int) = Integer.parseInt(digits.toString, b).toByte
    def b = base(2)
    def o = base(8)
    def x = base(16)
  }
}

import Extensions._

sealed trait Harmonics
case object Low extends Harmonics
case object High extends Harmonics

sealed trait Voxel
case object Full extends Voxel
case object Void extends Voxel

case class State(
  energy: Int,
  harmonics: Harmonics,
  matrix: Matrix,
  bots: Set[Bot],
  trace: List[Command])

case class Bot(
  bid: Int,
  pos: Coord,
  seeds: Set[Int])

case class Coord(x: Int, y: Int, z: Int)

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
  val encoded = Vector((100 + lld.a.encoded << 4).toByte, (lld.i + 15).toByte)
}

case class LMove(sld1: SLD, sld2: SLD) extends Command {
  val encoded = Vector((1100 + sld1.a.encoded << 4 + sld2.a.encoded << 6).toByte, (sld1.i + sld2.i << 4).toByte)
}

case class FusionP(nd: NCD) extends Command {
  val encoded = Vector((111 + nd.encoded << 3).toByte)
}

case class FusionS(nd: NCD) extends Command {
  val encoded = Vector((110 + nd.encoded << 3).toByte)
}

case class Fission(nd: NCD, m: Int) extends Command {
  val encoded = Vector((101 + nd.encoded << 3).toByte, m.toByte)
}

case class Fill(nd: NCD) extends Command {
  val encoded = Vector((11 + nd.encoded << 3).toByte)
}

case class LLD(a: Dir, i: Int) {
  require(i >= -15 && i <= 15)
}

case class SLD(a: Dir, i: Int) {
  require(i >= -5 && i <= 5)
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
