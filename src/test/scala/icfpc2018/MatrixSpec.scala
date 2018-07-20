package icfpc2018

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
  }
}
