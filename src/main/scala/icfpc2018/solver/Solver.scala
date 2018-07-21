package icfpc2018.solver

import icfpc2018._

trait Solver {
  def solve(model: Matrix): List[Command]
}
