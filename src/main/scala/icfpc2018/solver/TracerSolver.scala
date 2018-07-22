package icfpc2018.solver

import scala.collection.mutable

import icfpc2018._
import icfpc2018.solver.SolverDSL.{ ReleaseHarmonics, RequireHarmonics, SolverCommand }

object TracerSolver extends PartialSolver {
  def partialSolve(srcModel: Matrix, dstModel: Matrix, from: Coord): (List[SolverCommand], Matrix, Coord) = {
    var dx = 1
    var dz = 1
    var x = from.x
    var y = from.y
    var z = from.z
    val commands = mutable.ListBuffer[SolverCommand]()
    val maxX = (srcModel.voxels ++ dstModel.voxels).maxBy(_.x).x
    val maxY = (srcModel.voxels ++ dstModel.voxels).maxBy(_.y).y
    val maxZ = (srcModel.voxels ++ dstModel.voxels).maxBy(_.z).z
    val dimension = math.max(maxX, maxZ) + 1
    val len = dimension * dimension * (maxY + 1)
    var currModel = srcModel
    var highHarmonics = false

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
        if (currModel.isGrounded && highHarmonics) {
          highHarmonics = false
          commands += Void(NCD(nx - x, ny - y, nz - z))
          commands += ReleaseHarmonics
        } else if (!currModel.isGrounded && !highHarmonics) {
          highHarmonics = true
          commands += RequireHarmonics
          commands += Void(NCD(nx - x, ny - y, nz - z))
        } else commands += Void(NCD(nx - x, ny - y, nz - z))
      }
      commands += SMove(LLD(dir, diff))
      if (dstModel.get(currentCoord) == Full) {
        currModel = currModel.fill(currentCoord)
        if (currModel.isGrounded && highHarmonics) {
          highHarmonics = false
          commands += Fill(NCD(x - nx, y - ny, z - nz))
          commands += ReleaseHarmonics
        } else if (!currModel.isGrounded && !highHarmonics) {
          highHarmonics = true
          commands += RequireHarmonics
          commands += Fill(NCD(x - nx, y - ny, z - nz))
        } else commands += Fill(NCD(x - nx, y - ny, z - nz))
      }

      x = nx
      y = ny
      z = nz
    }
    (commands.toList, dstModel, Coord(x, y, z))
  }
}
