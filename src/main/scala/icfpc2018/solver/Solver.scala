package icfpc2018.solver

import icfpc2018._
import icfpc2018.solver.pathing.AStarPathFinder
import icfpc2018.solver.SolverDSL._

trait Solver {
  def solve(model: Matrix): List[Command]
}

trait RebuilderSolver extends Solver {
  def solve(srcModel: Matrix, dstModel: Matrix): List[Command]
  def solve(model: Matrix): List[Command] = solve(Matrix(model.dimension), model)
}

trait PartialSolver extends RebuilderSolver {
  def partialSolve(srcModel: Matrix, dstModel: Matrix, from: Coord): (List[SolverCommand], Matrix, Coord)
  def solve(srcModel: Matrix, dstModel: Matrix): List[Command] = {
    val (baseSolution, currentModel, currentCoord) = partialSolve(srcModel, dstModel, Coord(0, 0, 0))

    val returnToBase: List[SolverCommand] = if (currentCoord != Coord(0, 0, 0)) {
      val pf = new AStarPathFinder(currentModel, Set())
      ReleaseHarmonics :: pf.findPath(currentCoord, Coord(0, 0, 0)).map(RawCommand)
    } else List(ReleaseHarmonics)

    val res = SolverDSL.toCommands(baseSolution ++ returnToBase ++ List(RawCommand(Halt)))
    res
  }
}
