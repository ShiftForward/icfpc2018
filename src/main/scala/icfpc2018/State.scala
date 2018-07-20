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

case object Halt {
  val encoded = Vector(0xFF)
}

case object Wait {
  val encoded = Vector(0xFE)
}

case object Flip {
  val encoded = Vector(0xFD)
}

case class SMove(lld: LLD) {
  val encoded = Vector()
}

case class LLD(a: Dir, dist: Int)

sealed trait Dir {
  def encoded: Byte
}

case object X extends Dir {
  val encoded = 1.b
}

case object Y extends Dir {
  val encoded = 0
}

case object Z extends Dir {
  val encoded = 0
}
