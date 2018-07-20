package icfpc2018.floodfill

import scala.collection.mutable

import icfpc2018._

class FloodFill(model: Matrix) {
  private val visited: mutable.Set[Coord] = mutable.Set()
  private var coords: List[Coord] = Nil

  def fill(coord: Coord): Unit = {
    if (coord.y < 0 || model.get(coord) == Void || visited(coord)) {
      return
    }
    coords = coord :: coords
    visited += coord
    coord.neighbors.foreach(fill)
  }
}

object FloodFill {

  // returns the list of coordinates to fill (pos-order traversal)
  def fill(root: Coord, model: Matrix): List[Coord] = {
    val ff = new FloodFill(model)
    ff.fill(root)
    ff.coords.reverse
  }
}
