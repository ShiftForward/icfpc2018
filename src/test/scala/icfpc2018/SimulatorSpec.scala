package icfpc2018

import java.io.File

import icfpc2018.solver.TracerSolver
import icfpc2018.solver.floodfill.FloodFillSolver
import org.specs2.mutable.Specification

class SimulatorSpec extends Specification {

  "A Simulator" should {

    "simulate a Trace solver" in {
      val model = Matrix.fromMdl(new File("models/lightning/LA001_tgt.mdl"))
      val cmds = TracerSolver.solve(model)

      val st = Simulator.runAndValidate(model, cmds)
      st.energy === 2043360468L
    }

    "simulate a Flood Fill solver" in {
      val model = Matrix.fromMdl(new File("models/lightning/LA001_tgt.mdl"))
      val cmds = FloodFillSolver().solve(model)

      val st = Simulator.runAndValidate(model, cmds)
      st.energy === 236286008L
    }
  }
}
