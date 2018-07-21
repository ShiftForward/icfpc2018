package icfpc2018.solver.floodfill

import icfpc2018._
import icfpc2018.solver.Solver
import icfpc2018.solver.pathing.BfsPathFinder

// with pre-order (no need for high harmonics) this always seems to get stuck :(
case class FloodFillSolver(postOrder: Boolean = true) extends Solver {

  def solve(model: Matrix): List[Command] = {
    def aux(coords: List[Coord], auxState: State): List[Command] = {
      assert(
        auxState.bots.head.pos == coords.head,
        s"Bot should be at ${coords.head}, but state was:\n$auxState")

      coords match {
        case c1 :: c2 :: rest if c1.neighbors.contains(c2) =>
          val newMoves = SMove(LLD.forMove(c1, c2).get) :: fillFor(auxState, NCD.forMove(c2, c1).get, c1)
          val newAuxState = Simulator.run(auxState.copy(trace = newMoves), model)
          newMoves ::: aux(c2 :: rest, newAuxState)

        case c1 :: c2 :: rest =>
          BfsPathFinder.findPath(c1, c2, auxState.matrix) match {
            case None => throw new Exception(s"Can't find way from $c1 to $c2 :(")
            case Some(SMove(lld) :: pathRest) =>
              val newMoves = SMove(lld) :: fillFor(auxState, NCD.forMove(c1 + lld, c1).get, c1) ::: pathRest
              val newAuxState = Simulator.run(auxState.copy(trace = newMoves), model)
              newMoves ::: aux(c2 :: rest, newAuxState)
          }

        case c1 :: Nil =>
          BfsPathFinder.findPath(c1, Coord(0, 0, 0), auxState.matrix) match {
            case None => throw new Exception(s"Can't find way from $c1 to 0,0,0 :(")
            case Some(SMove(lld) :: pathRest) =>
              SMove(lld) :: fillFor(auxState, NCD.forMove(c1 + lld, c1).get, c1) ::: pathRest
          }
      }
    }

    def fillFor(auxState: State, ncd: NCD, coord: Coord): List[Command] = {
      val newMatrix = auxState.matrix.fill(coord)

      if (newMatrix.isGrounded && auxState.harmonics == High) List(Fill(ncd), Flip)
      else if (auxState.harmonics == Low && !newMatrix.isGrounded) List(Flip, Fill(ncd))
      else List(Fill(ncd))
    }

    val initialState = Simulator.run(model, Nil)
    val groundVoxel = model.voxels.filter(_.y == 0).head
    val fillStrategy = FloodFill.fill(groundVoxel, model, postOrder)

    val initialMoves = BfsPathFinder.findPath(Coord(0, 0, 0), fillStrategy.head, initialState.matrix).get
    val newAuxState = Simulator.run(initialState.copy(trace = initialMoves), model)
    initialMoves ::: aux(fillStrategy, newAuxState) ::: Halt :: Nil
  }
}
