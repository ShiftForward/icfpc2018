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

      val cutMatrixBottom = groundedMatrix.void(Coord(1, 0, 1))
      println(cutMatrixBottom.voxels)
      cutMatrixBottom.isGrounded === false
      cutMatrixBottom.voxels.size === 2
      cutMatrixBottom.groundedVoxels.size === 0
      cutMatrixBottom.ungroundedVoxels.size === 2

      val cutMatrixMiddle = groundedMatrix.void(Coord(1, 1, 1))
      cutMatrixMiddle.isGrounded === false
      cutMatrixMiddle.voxels.size === 2
      cutMatrixMiddle.groundedVoxels.size === 1
      cutMatrixMiddle.ungroundedVoxels.size === 1
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

    "correctly check if a cube is totally full or void" in {
      val matrix2 = Matrix(2)
      matrix2.isVoidOrFull(Coord(0, 0, 0), Coord(1, 1, 1)) must beSome(Void)

      // a 4x4 matrix
      val matrix4 = Matrix(4)
        .fill(Coord(1, 0, 1))
        // with a 2x2 cube
        .fill(Coord(1, 1, 1))
        .fill(Coord(2, 1, 1))
        .fill(Coord(1, 2, 1))
        .fill(Coord(2, 2, 1))
        .fill(Coord(1, 1, 2))
        .fill(Coord(2, 1, 2))
        .fill(Coord(1, 2, 2))
        .fill(Coord(2, 2, 2))

      matrix4.isVoidOrFull(Coord(0, 0, 0), Coord(3, 3, 3)) must beNone
      matrix4.isVoidOrFull(Coord(1, 1, 1), Coord(2, 2, 2)) must beSome(Full)
      matrix4.isVoidOrFull(Coord(1, 0, 1), Coord(1, 0, 1)) must beSome(Full)
      matrix4.isVoidOrFull(Coord(0, 0, 1), Coord(0, 0, 1)) must beSome(Void)
      matrix4.isVoidOrFull(Coord(0, 0, 0), Coord(3, 0, 0)) must beSome(Void)
    }
  }
}
