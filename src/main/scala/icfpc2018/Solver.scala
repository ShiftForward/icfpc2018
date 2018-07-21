package icfpc2018

import scala.collection.mutable

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

    commands += Flip
    commands += Halt
    commands.toList
  }
}

object GreedySolver extends Solver {
  def solve(model: Matrix): List[Command] = {
    val toPaint = model.voxels.groupBy(_.y)
    val commands = mutable.ListBuffer[Command]()

    var currentCoord = Coord(0, 0, 0)
    var currentModel = Matrix(model.dimension)
    var flipped = false

    toPaint.toList.sortBy(_._1).foreach {
      case (_, points) =>
        val nextToPaint = points.toList.sortBy(currentCoord.manhattanDistanceTo)
        nextToPaint.foreach { coord =>
          if (flipped && currentModel.isGrounded) {
            commands += Flip
            flipped = false
          }

          val pf = new AStarPathFinder(currentModel)

          val coordToMove = coord.copy(y = coord.y + 1)
          commands ++= pf.findPath(currentCoord, coordToMove)
          if (flipped || currentModel.supported(coord)) {
            commands += Fill(NCD(0, -1, 0))
          } else {
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
