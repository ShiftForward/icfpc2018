package icfpc2018.floodfill

import icfpc2018._

// this always seems to fail stuck :(
object FloodFillSolver extends Solver {

  def solve(model: Matrix): List[Command] = {
    val cmds = fillCommands(model)
    cmds.foreach(println)
    cmds
  }

  def fillCommands(model: Matrix): List[Command] = {

    def aux(coords: List[Coord], auxState: State): List[Command] = coords match {
      case c1 :: c2 :: rest if c1.neighbors.contains(c2) =>
        val newMoves = SMove(LLD.forMove(c1, c2).get) :: Fill(NCD.forMove(c2, c1).get) :: Nil
        val newAuxState = Simulator.run(auxState.copy(trace = newMoves), model)
        newMoves ::: aux(c2 :: rest, newAuxState)

      case c1 :: c2 :: rest =>
        PathFinder.findPath(c1, c2, auxState.matrix) match {
          case None => throw new Exception(s"I am stuck :(\n$c1 to $c2")
          case Some(SMove(lld) :: pathRest) =>
            val newMoves = SMove(lld) :: Fill(NCD.forMove(c1 + lld, c1).get) :: pathRest
            val newAuxState = Simulator.run(auxState.copy(trace = newMoves), model)
            newMoves ::: aux(c2 :: rest, newAuxState)
        }

      case c1 :: Nil =>
        PathFinder.findPath(c1, Coord(0, 0, 0), auxState.matrix) match {
          case None => throw new Exception(s"I am stuck :(\n$c1 to 0,0,0")
          case Some(SMove(lld) :: pathRest) =>
            SMove(lld) :: Fill(NCD.forMove(c1 + lld, c1).get) :: pathRest
        }
    }

    val initialState = Simulator.run(model, Nil)
    val groundVoxel = model.voxels.filterKeys(_.y == 0).head._1
    val fillStrategy = fill(groundVoxel, model)

    val newMoves = PathFinder.findPath(Coord(0, 0, 0), groundVoxel, initialState.matrix).get
    val newAuxState = Simulator.run(initialState.copy(trace = newMoves), model)
    newMoves ::: aux(fillStrategy, newAuxState)
  }
}
