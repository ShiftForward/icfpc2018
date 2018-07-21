package icfpc2018.solver

import scala.collection.mutable

import icfpc2018._
import icfpc2018.solver.pathing.AStarPathFinder

object GreedySolver extends Solver {
  def solve(model: Matrix): List[Command] = {
    val toPaint = model.voxels.groupBy(_.y)
    val commands = mutable.ListBuffer[Command]()

    var currentCoord = Coord(0, 0, 0)
    var currentModel = Matrix(model.dimension)
    var flipped = false

    toPaint.toList.sortBy(_._1).foreach {
      case (y, points) =>
        val nextToPaint = points.toList.sortBy(currentCoord.manhattanDistanceTo)
        nextToPaint.foreach { coord =>
          if (flipped && currentModel.isGrounded) {
            commands += Flip
            flipped = false
          }

          val pf = new AStarPathFinder(currentModel)

          val coordToMove = coord.copy(y = coord.y + 1)
          commands ++= pf.findPath(currentCoord, coordToMove)
          if (flipped || currentModel.supported(coord))
            commands += Fill(NCD(0, -1, 0))
          else {
            commands += Flip
            commands += Fill(NCD(0, -1, 0))
            flipped = true
          }

          currentCoord = coordToMove
          currentModel = currentModel.fill(coord)
        }
    }

    if (flipped)
      commands += Flip

    val pf = new AStarPathFinder(currentModel)

    commands ++= pf.findPath(currentCoord, Coord(0, 0, 0))
    commands += Halt

    commands.toList
  }
}
