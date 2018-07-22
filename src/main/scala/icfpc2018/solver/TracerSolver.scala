package icfpc2018.solver

import scala.collection.mutable

import icfpc2018._
import icfpc2018.solver.SolverDSL.{ RawCommand, ReleaseHarmonics, RequireHarmonics, SolverCommand }
import icfpc2018.solver.pathing.AStarPathFinder

object TracerSolver extends PartialSolver {
  def partialSolve(srcModel: Matrix, dstModel: Matrix, from: Coord): (List[SolverCommand], Matrix, Coord) = {
    var dx = 1
    var dz = 1
    val commands = mutable.ListBuffer[SolverCommand]()

    val minX = (srcModel.voxels ++ dstModel.voxels).minBy(_.x).x
    val minZ = (srcModel.voxels ++ dstModel.voxels).minBy(_.z).z

    val maxX = (srcModel.voxels ++ dstModel.voxels).maxBy(_.x).x
    val maxY = (srcModel.voxels ++ dstModel.voxels).maxBy(_.y).y
    val maxZ = (srcModel.voxels ++ dstModel.voxels).maxBy(_.z).z

    val len = (maxX + 1 - (minX - 1)) * (maxY + 1) * (maxZ + 1 - (minZ - 1))

    commands ++=
      new AStarPathFinder(srcModel, Set()).findPath(from, Coord(minX - 1, 0, minZ - 1)).map(RawCommand)

    var x = minX - 1
    var y = 0
    var z = minZ - 1

    var currModel = srcModel
    var highHarmonics = false

    (0 until len).foreach { _ =>
      val currentCoord = Coord(x, y, z)
      var nz = z
      var ny = y
      var nx = x
      nz += dz
      if (nz >= (maxZ + 1) || nz < (minZ - 1)) {
        nz = if (nz == (maxZ + 1)) maxZ else (minZ - 1)
        nx += dx
        dz = if (dz == 1) -1 else 1
      }

      if (nx >= (maxX + 1) || nx < (minX - 1)) {
        nx = if (nx == (maxX + 1)) maxX else (minX - 1)
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
