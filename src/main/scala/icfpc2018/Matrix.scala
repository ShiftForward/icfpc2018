package icfpc2018

import java.io.File
import java.nio.file.Files

case class Matrix(dimension: Int, voxels: Map[Coord, Voxel] = Map.empty) {

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

object Matrix {
  def fromMdl(mdlFile: File): Matrix = {
    val bytes: Array[Byte] = Files.readAllBytes(mdlFile.toPath)
    val matrix = Matrix(0xFF & bytes(0).asInstanceOf[Int])
    var z = 0
    var x = 0
    var y = 0
    bytes.drop(1).foldLeft(matrix) {
      case (m, b) =>
        (0 until 8).foldLeft(m) {
          case (m, i) =>
            val nextM = if ((b & (1 << i)) != 0)
              m.fill(Coord(x, y, z))
            else
              m

            z += 1
            if (z >= matrix.dimension) {
              z = 0
              y += 1
            }

            if (y >= matrix.dimension) {
              y = 0
              x += 1
            }

            nextM
        }
    }
  }
}
