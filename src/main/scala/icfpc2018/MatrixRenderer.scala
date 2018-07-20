package icfpc2018

object MatrixRenderer {
  def renderSlices(matrix: Matrix, step: Int = 1, sleep: Long = 500): Unit = {
    def renderZ(z: Int): Unit = {
      ((matrix.dimension - 1) to 0 by -step).foreach { y =>
        (0 until matrix.dimension by step).foreach { x =>
          if (matrix.get(Coord(x, y, z)) == Full) print('$') else print('.')
        }
        println()
      }
      println("Z: " + z)
    }

    (0 until matrix.dimension by step).foreach { z =>
      renderZ(z)
      Thread.sleep(sleep)
      println("\033[H\033[2J")
    }
  }

  def renderZBuffer(matrix: Matrix, step: Int = 1, sleep: Long = 500): Unit = {

    def symbol(z: Int) = symbolRelative(z.toDouble / matrix.dimension)

    def symbolRelative(z: Double) =
      if (z < 0.1) '.'
      else if (z < 0.2) '-'
      else if (z < 0.5) '+'
      else if (z < 0.7) '*'
      else if (z < 0.9) '#'
      else '$'

    val zBuffer: Array[Array[Int]] = Array.fill(matrix.dimension)(Array.fill(matrix.dimension)(0))
    def accumulateZ(z: Int): Unit = {
      (0 until matrix.dimension by step).foreach { y =>
        (0 until matrix.dimension by step).foreach { x =>
          if (matrix.get(Coord(x, y, z)) == Full) {
            zBuffer(y)(x) = matrix.dimension - z
          }
        }
      }
    }

    ((matrix.dimension - 1) to 0 by -1).foreach { z =>
      accumulateZ(z)
    }
    println(zBuffer.reverse.map(_.map(symbol).mkString).mkString("\n"))
  }
}
