package icfpc2018.solver

import scala.collection.mutable

import icfpc2018._

object TracerSolver extends Solver {
  def baseSolve(model: Matrix, from: Coord): (List[Command], Matrix, Coord) = {
    var dx = 1
    var dz = 1
    var x = from.x
    var y = from.y
    var z = from.z
    val commands = mutable.ListBuffer[Command]()
    val len = model.dimension * model.dimension * model.dimension
    commands += Flip

    (0 until len - 1).foreach { _ =>
      val currentCoord = Coord(x, y, z)
      var nz = z
      var ny = y
      var nx = x
      nz += dz
      if (nz >= model.dimension || nz < 0) {
        nz = if (nz == model.dimension) model.dimension - 1 else 0
        nx += dx
        dz = if (dz == 1) -1 else 1
      }

      if (nx >= model.dimension || nx < 0) {
        nx = if (nx == model.dimension) model.dimension - 1 else 0
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

      commands += SMove(LLD(dir, diff))
      if (model.get(currentCoord) == Full) {
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
    (commands.toList, model, Coord(0, 0, 0))
  }
}
