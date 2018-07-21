package icfpc2018.solver

import scala.collection.mutable.ListBuffer

import icfpc2018.{ Command, Flip, Wait }

object SolverDSL {
  sealed trait SolverCommand
  case class RawCommand(command: Command) extends SolverCommand
  case object RequireHarmonics extends SolverCommand
  case object ReleaseHarmonics extends SolverCommand

  implicit def commandConversion(cmd: Command): SolverCommand = RawCommand(cmd)

  def toCommands(commands: List[SolverCommand]): List[Command] = {
    val genCommands = ListBuffer[Command]()
    var requireHarmonics: Boolean = false
    commands.foreach {
      case RawCommand(cmd) =>
        genCommands += cmd
        if (cmd == Flip) requireHarmonics = !requireHarmonics
      case RequireHarmonics if !requireHarmonics =>
        requireHarmonics = true
        genCommands += Flip
      case ReleaseHarmonics if requireHarmonics =>
        requireHarmonics = false
        genCommands += Flip
      case _ => // Do nothing
    }
    genCommands.toList
  }

  def toFlatCommands(commands: List[List[SolverCommand]]): List[Command] = {
    val genCommands = ListBuffer[Command]()
    var harmonicsLock: Array[Boolean] = Array.fill(commands.size)(false)
    def requireHarmonics: Boolean = harmonicsLock.exists(identity)
    var remainingCommands: Seq[List[SolverCommand]] = commands
    while (remainingCommands.flatten.nonEmpty) {
      val nextHarmonicsLock = harmonicsLock.clone()
      val nextCommands: Seq[Command] = remainingCommands.map(_.headOption).zipWithIndex.map {
        case (Some(RawCommand(cmd)), idx) =>
          if (cmd == Flip) nextHarmonicsLock(idx) = !nextHarmonicsLock(idx)
          cmd
        case (Some(RequireHarmonics), idx) =>
          nextHarmonicsLock(idx) = true
          Wait
        case (Some(ReleaseHarmonics), idx) =>
          nextHarmonicsLock(idx) = false
          Wait
        case _ =>
          Wait
      }
      genCommands ++= nextCommands
      val nextRequireHarmonics = nextHarmonicsLock.exists(identity)
      if (nextRequireHarmonics != requireHarmonics) {
        genCommands += Flip
        genCommands ++= List.fill(nextCommands.size - 1)(Wait)
      }
      harmonicsLock = nextHarmonicsLock
      remainingCommands = remainingCommands.map(_.drop(1))
    }
    genCommands.toList
  }

}
