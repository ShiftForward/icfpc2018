package icfpc2018.solver

import scala.collection.mutable

import icfpc2018._
import icfpc2018.solver.pathing.AStarPathFinder
import icfpc2018.solver.SolverDSL._

object GreedySolver extends SimpleSolver {
  def baseSolve(model: Matrix, from: Coord): (List[SolverCommand], Matrix, Coord) = {
    val toPaint = model.voxels.groupBy(_.y)
    val commands = mutable.ListBuffer[SolverCommand]()

    var currentCoord = from
    var currentModel = Matrix(model.dimension)
    var requestedHarmonics = false

    toPaint.toList.sortBy(_._1).foreach {
      case (y, points) =>
        var pointsToPaint = points

        while (pointsToPaint.nonEmpty) {
          val grounded = pointsToPaint.filter(currentModel.supported)
          val nextToPaint = if (grounded.nonEmpty)
            grounded.toList.minBy(_.manhattanDistanceTo(Coord(0, 0, 0)))
          else
            pointsToPaint.toList.minBy(_.manhattanDistanceTo(Coord(0, 0, 0)))

          if (requestedHarmonics && currentModel.isGrounded) {
            commands += ReleaseHarmonics
            requestedHarmonics = false
          }

          val pf = new AStarPathFinder(currentModel)

          val coordToMove = nextToPaint.copy(y = nextToPaint.y + 1)
          commands ++= pf.findPath(currentCoord, coordToMove).map(RawCommand)
          if (requestedHarmonics || currentModel.supported(nextToPaint))
            commands += Fill(NCD(0, -1, 0))
          else {
            commands += RequireHarmonics
            requestedHarmonics = true
            commands += Fill(NCD(0, -1, 0))
          }

          currentCoord = coordToMove
          currentModel = currentModel.fill(nextToPaint)
          pointsToPaint = pointsToPaint - nextToPaint
        }
    }
    (commands.toList, currentModel, currentCoord)
  }
}
