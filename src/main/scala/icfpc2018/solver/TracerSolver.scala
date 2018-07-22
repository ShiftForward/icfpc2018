package icfpc2018.solver

import scala.collection.mutable

import icfpc2018._
import icfpc2018.solver.SolverDSL.{ ReleaseHarmonics, RequireHarmonics, SolverCommand }

object TracerSolver extends PartialSolver {
  def partialSolve(srcModel: Matrix, dstModel: Matrix, from: Coord): (List[SolverCommand], Matrix, Coord) = {
    val dimension = dstModel.dimension
    var dx = 1
    var dz = 1
    var x = from.x
    var y = from.y
    var z = from.z
    val commands = mutable.ListBuffer[SolverCommand]()
    val maxY = math.max(srcModel.voxels.maxBy(_.y).y, dstModel.voxels.maxBy(_.y).y)
    val len = dimension * dimension * (maxY + 1)
    var currModel = srcModel
    var highHarmonics = false

    def updateHarmonics() =
      if (currModel.isGrounded && highHarmonics) commands += ReleaseHarmonics
      else if (!currModel.isGrounded && !highHarmonics) commands += ReleaseHarmonics

    (0 until len).foreach { _ =>
      val currentCoord = Coord(x, y, z)
      var nz = z
      var ny = y
      var nx = x
      nz += dz
      if (nz >= dimension || nz < 0) {
        nz = if (nz == dimension) dimension - 1 else 0
        nx += dx
        dz = if (dz == 1) -1 else 1
      }

      if (nx >= dimension || nx < 0) {
        nx = if (nx == dimension) dimension - 1 else 0
        ny += 1
        dx = if (dx == 1) -1 else 1
      }

      val (dir: Dir, diff: Int) = if (nx - x != 0) {
        (X, nx - x)
      } else if (ny - y != 0) {
        (Y, ny - y)
      } else if (nz - z != 0) {
        (Z, nz - z)
      }

      if (srcModel.get(Coord(nx, ny, nz)) == Full) {
        currModel = currModel.void(Coord(nx, ny, nz))
        updateHarmonics()
        commands += Void(NCD(nx - x, ny - y, nz - z))
      }
      commands += SMove(LLD(dir, diff))
      if (dstModel.get(currentCoord) == Full) {
        currModel = currModel.fill(currentCoord)
        updateHarmonics()
        commands += Fill(NCD(x - nx, y - ny, z - nz))
      }

      x = nx
      y = ny
      z = nz
    }

    while (z != 0) {
      val dist = math.max(0 - z, -15)
      commands += SMove(LLD(Z, dist))
      z += dist
    }

    while (x != 0) {
      val dist = math.max(0 - x, -15)
      commands += SMove(LLD(X, dist))
      x += dist
    }

    while (y != 0) {
      val dist = math.max(0 - y, -15)
      commands += SMove(LLD(Y, dist))
      y += dist
    }
    (commands.toList, dstModel, Coord(0, 0, 0))
  }
}
