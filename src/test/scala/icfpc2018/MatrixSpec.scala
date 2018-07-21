package icfpc2018

import scala.util.Random

import icfpc2018.Matrix.CoordSet
import org.specs2.mutable.Specification

class MatrixSpec extends Specification {
  "A Matrix" should {
    "correctly check if all voxels are grounded" in {
      val emptyMatrix = Matrix(5)
      emptyMatrix.isGrounded === true
      emptyMatrix.voxels.size === 0
      emptyMatrix.groundedVoxels.size === 0
      emptyMatrix.ungroundedVoxels.size === 0

      val floatingMatrix = emptyMatrix.fill(Coord(1, 1, 1)).fill(Coord(1, 2, 1))
      floatingMatrix.isGrounded === false
      floatingMatrix.voxels.size === 2
      floatingMatrix.groundedVoxels.size === 0
      floatingMatrix.ungroundedVoxels.size === 2

      val groundedMatrix = floatingMatrix.fill(Coord(1, 0, 1))
      groundedMatrix.isGrounded === true
      groundedMatrix.voxels.size === 3
      groundedMatrix.groundedVoxels.size === 3
      groundedMatrix.ungroundedVoxels.size === 0
    }

    "have an optimized CoordSet" in {
      val randomCoords = (1 to 100).map { _ =>
        Coord(Random.nextInt(250), Random.nextInt(250), Random.nextInt(250))
      }.toSet
      val coordSet = randomCoords.foldLeft(CoordSet())(_ + _)

      coordSet.iterator.toSet === randomCoords

      val (coordsToRemove, coordsToKeep) = randomCoords.splitAt(25)
      val coordSetToRemove = coordsToRemove.foldLeft(CoordSet())(_ + _)

      val coordSetToKeep = coordSet -- coordSetToRemove
      coordSetToKeep.iterator.toSet === coordsToKeep

      (coordSetToKeep ++ coordSetToRemove).iterator.toSet === randomCoords

    }
  }
}
