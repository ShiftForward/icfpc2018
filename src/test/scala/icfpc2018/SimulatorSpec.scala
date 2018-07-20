package icfpc2018

import java.io.File

import org.specs2.mutable.Specification

class SimulatorSpec extends Specification {

  "A Simulator" should {

    "be" in {
      val model = Matrix.fromMdl(new File("models/lightning/LA001_tgt.mdl"))
      val cmds = Tracer.solve(model)

      val st = Simulator.runAndValidate(model, cmds)
      st.energy === 2043360468L
    }
  }
}
