package icfpc2018.solver.floodfill

import scala.collection.mutable

import icfpc2018._

class FloodFill(model: Matrix, postOrder: Boolean) {
  private val visited: mutable.Set[Coord] = mutable.Set()
  private var coords: List[Coord] = Nil

  def fill(coord: Coord): Unit = {
    if (coord.y < 0 || model.get(coord) == Void || visited(coord)) {
      return
    }
    if (!postOrder) {
      coords = coord :: coords
    }
    visited += coord
    coord.neighbors.foreach(fill)

    if (postOrder) {
      coords = coord :: coords
    }
  }
}

object FloodFill {

  // returns the list of coordinates to fill
  def fill(root: Coord, model: Matrix, postOrder: Boolean = false): List[Coord] = {
    val ff = new FloodFill(model, postOrder)
    ff.fill(root)
    ff.coords.reverse
  }
}
