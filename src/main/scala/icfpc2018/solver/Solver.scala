package icfpc2018.solver

import icfpc2018._
import icfpc2018.solver.pathing.AStarPathFinder

trait Solver {
  def baseSolve(model: Matrix, from: Coord): (List[Command], Matrix, Coord)
  def solve(model: Matrix): List[Command] = {
    val (baseSolution, currentModel, currentCoord) = baseSolve(model, Coord(0, 0, 0))

    val returnToBase: List[Command] = if (currentCoord != Coord(0, 0, 0)) {
      val pf = new AStarPathFinder(currentModel)
      pf.findPath(currentCoord, Coord(0, 0, 0))
    } else Nil

    val tail = if (baseSolution.count(_ == Flip) % 2 == 1) Flip :: (returnToBase ++ List(Halt))
    else List(Halt)

    baseSolution ++ tail
  }
}
