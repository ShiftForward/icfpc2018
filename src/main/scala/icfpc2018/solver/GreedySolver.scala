package icfpc2018.solver

import scala.collection.mutable

import icfpc2018._
import icfpc2018.solver.pathing.AStarPathFinder
import icfpc2018.solver.SolverDSL._

object GreedySolver extends PartialSolver {
  def partialSolve(srcModel: Matrix, dstModel: Matrix, from: Coord): (List[SolverCommand], Matrix, Coord) = {
    val dimensions = dstModel.dimension
    val toPaint = (dstModel.voxels -- srcModel.voxels).groupBy(_.y)
    val commands = mutable.ListBuffer[SolverCommand]()

    var currentCoord = from
    var currentModel = Matrix(dimensions)
    var requestedHarmonics = false

    commands += SMove(LLD(Y, 1))
    currentCoord = currentCoord.copy(y = currentCoord.y + 1)

    toPaint.toList.sortBy(_._1).foreach {
      case (y, points) =>
        var pointsToPaint = points

        while (pointsToPaint.nonEmpty) {
          val grounded = pointsToPaint.filter(currentModel.supported)
          val nextToPaint =
            if (grounded.nonEmpty) zigZagOnX(currentCoord, grounded.toList)
            else zigZagOnX(currentCoord, pointsToPaint.toList)

          if (requestedHarmonics && currentModel.isGrounded) {
            commands += ReleaseHarmonics
            requestedHarmonics = false
          }

          val pf = new AStarPathFinder(currentModel, Set())

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

        if (currentCoord.y + 1 < dimensions) {
          commands += SMove(LLD(Y, 1))
          currentCoord = currentCoord.copy(y = currentCoord.y + 1)
        }
    }
    (commands.toList, currentModel, currentCoord)
  }

  def zigZagOnX(current: Coord, pointsToPaint: List[Coord]): Coord = {
    val (_, points) = pointsToPaint.groupBy(_.x).minBy(_._1)
    val minPt = points.minBy(_.z)
    val maxPt = points.maxBy(_.z)
    if (math.abs(minPt.z - current.z) <= math.abs(maxPt.z - current.z)) minPt else maxPt
  }
}
