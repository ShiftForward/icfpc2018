package icfpc2018.solver

import scala.collection.mutable

import icfpc2018._
import icfpc2018.solver.pathing.AStarPathFinder

object GreedySolver extends Solver {
  def baseSolve(model: Matrix, from: Coord): (List[Command], Matrix, Coord) = {
    val toPaint = model.voxels.groupBy(_.y)
    val commands = mutable.ListBuffer[Command]()

    var currentCoord = from
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
    (commands.toList, currentModel, currentCoord)
  }
}
