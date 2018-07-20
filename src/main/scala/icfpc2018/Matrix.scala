package icfpc2018

case class Matrix(dimension: Int, voxels: Map[Coord, Voxel]) {

  def validateCoord(coord: Coord): Boolean =
    coord.x >= 0 && coord.x < dimension &&
      coord.y >= 0 && coord.y < dimension &&
      coord.z >= 0 && coord.z < dimension

  def canFillCoord(coord: Coord): Boolean =
    coord.x >= 1 && coord.x < (dimension - 1) &&
      coord.y >= 0 && coord.y < dimension &&
      coord.z >= 1 && coord.z < (dimension - 1) &&
      voxels.getOrElse(coord, Void) == Void

  def get(coord: Coord): Voxel = {
    require(validateCoord(coord), s"Invalid coordinate: $coord")
    voxels.getOrElse(coord, Void)
  }

  def fill(coord: Coord): Matrix = {
    require(canFillCoord(coord), s"Can't fill coordinate: $coord")
    copy(voxels = voxels.updated(coord, Full))
  }

}
