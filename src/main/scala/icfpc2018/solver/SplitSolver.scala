package icfpc2018.solver
import scala.collection.mutable

import icfpc2018.solver.SplitSolver._
import icfpc2018.solver.pathing.AStarPathFinder
import icfpc2018._
import icfpc2018.solver.SolverDSL.{ RawCommand, SolverCommand }

case class SplitSolver(innerSolver: SimpleSolver) extends Solver {
  def solve(model: Matrix): List[Command] = {

    val middle = model.centerOfMass
    val initialMiddle = middle.copy(y = 0) // NW
    val finalMiddle = middle.copy(y = model.dimension - 1)

    def quadrant(coord: Coord): Quadrant = {
      if (coord.x <= middle.x && coord.z >= middle.z) NW
      else if (coord.x >= middle.x && coord.z >= middle.z) NE
      else if (coord.x <= middle.x && coord.z <= middle.z) SW
      else if (coord.x >= middle.x && coord.z <= middle.z) SE
      else throw new Exception("Weird")
    }

    val emptyMatrix = Matrix(model.dimension)

    val quadrants = model.voxels.foldLeft((emptyMatrix, emptyMatrix, emptyMatrix, emptyMatrix)) {
      case ((nw, ne, sw, se), voxel) =>
        quadrant(voxel) match {
          case NW => (nw.fill(voxel), ne, sw, se)
          case NE => (nw, ne.fill(voxel), sw, se)
          case SW => (nw, ne, sw.fill(voxel), se)
          case SE => (nw, ne, sw, se.fill(voxel))
        }
    }

    val commands = mutable.ListBuffer[Command]()
    commands ++= new AStarPathFinder(emptyMatrix).findPath(Coord(0, 0, 0), initialMiddle)
    commands += Fission(NCD(1, 0, 0), 2) // NW -> NE
    commands += Fission(NCD(0, 0, -1), 1) // NW -> SW
    commands += Fission(NCD(0, 0, -1), 1) // NE -> SE

    def childCommands(quadrant: Matrix, startPos: Coord) = {
      val endPos = startPos.copy(y = model.dimension - 1)
      val (buildCommands, finalMatrix, finalCoord) =
        innerSolver.baseSolve(quadrant, startPos)
      val pf = new AStarPathFinder(finalMatrix)
      (buildCommands ++ pf.findPath(finalCoord, endPos).map(RawCommand),
        finalMatrix,
        endPos)
    }

    val childrenCommands = List(
      () => childCommands(quadrants._1, initialMiddle), // NW (1)
      () => childCommands(quadrants._2, initialMiddle.copy(x = initialMiddle.x + 1)), // NE (2)
      () => childCommands(quadrants._4, initialMiddle.copy(x = initialMiddle.x + 1, z = initialMiddle.z - 1)), // SE (3)
      () => childCommands(quadrants._3, initialMiddle.copy(z = initialMiddle.z - 1)) // SW (4)
    ).par.map(_.apply()).toList

    val expectedFinalModel = childrenCommands.flatMap(_._2.voxels.iterator).toSet.foldLeft(emptyMatrix) {
      case (m, v) =>
        m.fill(v)
    }

    val finalPf = new AStarPathFinder(expectedFinalModel)

    commands ++= SolverDSL.toFlatCommands(childrenCommands.map(_._1))

    commands += FusionP(NCD(1, 0, 0)) // NW (1)
    commands += FusionS(NCD(-1, 0, 0)) // NE (2)
    commands += FusionS(NCD(-1, 0, 0)) // SE (3)
    commands += FusionP(NCD(1, 0, 0)) // SW (4)

    commands += FusionP(NCD(0, 0, -1))
    commands += FusionS(NCD(0, 0, 1))

    if (commands.toList.count(_ == Flip) % 2 == 1)
      commands += Flip

    commands ++= finalPf.findPath(finalMiddle, Coord(0, 0, 0))

    commands += Halt

    commands.toList
  }
}

object SplitSolver {
  sealed trait Quadrant
  case object NW extends Quadrant
  case object NE extends Quadrant
  case object SW extends Quadrant
  case object SE extends Quadrant
}
