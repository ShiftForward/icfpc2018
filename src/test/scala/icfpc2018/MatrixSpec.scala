package icfpc2018

import org.specs2.mutable.Specification

class MatrixSpec extends Specification {
  "A Matrix" should {
    "correctly check if all voxels are grounded" in {
      val emptyMatrix = Matrix(5)
      val floatingMatrix = emptyMatrix.fill(Coord(1, 1, 1)).fill(Coord(1, 2, 1))
      val groundedMatrix = floatingMatrix.fill(Coord(1, 0, 1))

      emptyMatrix.isGrounded === true
      floatingMatrix.isGrounded === false
      groundedMatrix.isGrounded === true
    }
  }
}
