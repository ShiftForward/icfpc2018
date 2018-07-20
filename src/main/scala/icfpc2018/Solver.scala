package icfpc2018

import scala.collection.mutable.ListBuffer

trait Solver {
  def solve(model: Matrix): List[Command]
}

object Tracer extends Solver {
  def solve(model: Matrix): List[Command] = {
    var dx = 1
    var dz = 1
    var x = 0
    var y = 0
    var z = 0
    val commands = ListBuffer[Command]()
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

    commands += Flip
    commands += Halt
    commands.toList
  }
}
