package icfpc2018

import org.specs2.mutable.Specification

import Extensions._

class StateSpec extends Specification {

  "A State" should {

    "be correctly encoded" in {
      SMove(LLD(X, 12)).encoded === Vector(10100.b, 11011.b)
      SMove(LLD(Z, -4)).encoded === Vector(110100.b, 1011.b)
      LMove(SLD(X, 3), SLD(Y, -5)).encoded === Vector(10011100.b, 1000.b)
      LMove(SLD(Y, -2), SLD(Z, 2)).encoded === Vector(11101100.b, 1110011.b)
      FusionP(NCD(-1, 1, 0)).encoded === Vector(111111.b)
      FusionS(NCD(1, -1, 0)).encoded === Vector(10011110.b)
      Fission(NCD(0, 0, 1), 5).encoded === Vector(1110101.b, 101.b)
      Fill(NCD(0, -1, 0)).encoded === Vector(1010011.b)
    }
  }
}
