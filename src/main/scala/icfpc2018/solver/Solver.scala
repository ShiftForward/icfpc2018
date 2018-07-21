package icfpc2018.solver

import icfpc2018._
import icfpc2018.solver.pathing.AStarPathFinder
import icfpc2018.solver.SolverDSL._

trait Solver {
  def solve(model: Matrix): List[Command]
}

trait SimpleSolver extends Solver {
  def baseSolve(model: Matrix, from: Coord): (List[SolverCommand], Matrix, Coord)
  def solve(model: Matrix): List[Command] = {
    val (baseSolution, currentModel, currentCoord) = baseSolve(model, Coord(0, 0, 0))

    val returnToBase: List[SolverCommand] = if (currentCoord != Coord(0, 0, 0)) {
      val pf = new AStarPathFinder(currentModel)
      ReleaseHarmonics :: pf.findPath(currentCoord, Coord(0, 0, 0)).map(RawCommand)
    } else Nil

    SolverDSL.toCommands(baseSolution ++ returnToBase ++ List(RawCommand(Halt)))
  }
}
