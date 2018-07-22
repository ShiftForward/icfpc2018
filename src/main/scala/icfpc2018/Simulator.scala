package icfpc2018

import scala.collection.{ SortedSet, mutable }

object Simulator {

  case class SimulatorException(msg: String, st: State)
    extends Exception(s"$msg\nState: $st")

  case class CommandException(msg: String, cmd: Command, st: State, bot: Bot)
    extends Exception(s"$msg\nCommand: $cmd\nBot: $bot\nState: $st")

  case class ConflictingVolatileCoordsException(msg: String, volCoords: List[Coord], st: State)
    extends Exception(s"$msg\nVolatile coords: $volCoords\nState: $st")

  implicit val botOrdering: Ordering[Bot] = Ordering.by[Bot, Int](_.bid)

  def runAndValidate(model: Matrix, trace: List[Command]): State = {
    runAndValidate(initialState(model, trace), model)
  }

  def runAndValidate(st: State, model: Matrix): State = {
    val finalSt = run(st, model)
    if (finalSt.matrix.voxels != model.voxels) {
      throw SimulatorException("The final matrix does not match the provided model", st)
    }
    if (finalSt.bots.nonEmpty) {
      throw SimulatorException("The last command was not a Halt", st)
    }
    finalSt
  }

  def run(model: Matrix, trace: List[Command]): State = {
    run(initialState(model, trace), model)
  }

  def run(st: State, model: Matrix): State = {
    if (st.trace.isEmpty) st
    else run(nextTick(st, model), model)
  }

  private def nextTick(st: State, model: Matrix): State = {
    val harmonicsCost = if (st.harmonics == High) 30L else 3L
    val initEnergy = st.energy + harmonicsCost * model.dimension * model.dimension * model.dimension + 20 * st.bots.size
    val botSet = st.bots.toList
    val cmdSet = st.trace.take(botSet.size)

    val initialUpdSt = st.copy(energy = initEnergy, trace = st.trace.drop(botSet.size))

    val (newSt, allVolCoords) = processCmds(initialUpdSt, model, botSet, cmdSet)

    if (allVolCoords.length != allVolCoords.distinct.length) {
      throw ConflictingVolatileCoordsException(
        "Conflicting volatile coordinates",
        allVolCoords.diff(allVolCoords.distinct).distinct, newSt)
    }
    if (newSt.harmonics == Low && !newSt.matrix.isGrounded) {
      throw SimulatorException("Matrix is not grounded", newSt)
    }

    newSt
  }

  private def processCmds(initState: State, model: Matrix, currentBotSet: List[Bot], currentCmdSet: List[Command]): (State, List[Coord]) = {
    require(currentBotSet.size == currentCmdSet.size, "Bot set must be of same size as list of commands!")

    def sMoveAux(st: State, smove: SMove, bot: Bot): (State, List[Coord]) = {
      val cPrime = bot.pos + smove.lld
      val thisVolCoords = bot.pos.rangeTo(smove.lld)

      if (!st.matrix.validateCoord(cPrime)) {
        throw CommandException("c' is not valid", smove, st, bot)
      }
      thisVolCoords.foreach { coord =>
        if (st.matrix.get(coord) == Full) {
          throw CommandException(s"$coord is full", smove, st, bot)
        }
      }

      val newBot = bot.copy(pos = cPrime)
      (st.copy(bots = st.bots - bot + newBot), thisVolCoords)
    }

    def fillRegionAux(
      matrix: Matrix,
      from: Coord,
      to: Coord,
      energyCalc: (Matrix, Coord) => (Matrix, Long)): (Matrix, Long, List[Coord]) = {
      val xxs =
        if (from.x < to.x) from.x to to.x
        else to.x to from.x
      val yys =
        if (from.y < to.y) from.y to to.y
        else to.y to from.y
      val zzs =
        if (from.z < to.z) from.z to to.z
        else to.z to from.z

      val coords = for {
        x <- xxs
        y <- yys
        z <- zzs
      } yield Coord(x, y, z)

      val (m, e) = coords.foldLeft((matrix, 0l)) {
        case ((m, e), c) =>
          val (newM, ePlus) = energyCalc(m, c)
          (newM, e + ePlus)
      }
      (m, e, coords.toList)
    }

    val botCmdPairs = mutable.ListBuffer(currentBotSet.zip(currentCmdSet).toArray: _*)
    var endState = initState
    var endVolCoords = List.empty[Coord]

    while (botCmdPairs.nonEmpty) {
      val nextPair = botCmdPairs.head
      botCmdPairs -= nextPair
      val (nextBot: Bot, nextCmd) = nextPair

      nextCmd match {
        case Halt =>
          if (nextBot.pos != Coord(0, 0, 0) || endState.bots.size != 1 || endState.harmonics != Low) {
            throw CommandException("Invalid Halt", Halt, endState, nextBot)
          }
          endState = endState.copy(bots = SortedSet.empty[Bot])

        case Wait =>
          endVolCoords = nextBot.pos :: endVolCoords

        case Flip =>
          endState = endState.copy(harmonics = endState.harmonics.flipped)
          endVolCoords = nextBot.pos :: endVolCoords

        case smove @ SMove(lld) =>
          val (newState, newVolCoords) = sMoveAux(endState, smove, nextBot)
          endState = newState.copy(energy = endState.energy + 2 * math.abs(lld.len))
          endVolCoords = endVolCoords ::: newVolCoords

        case LMove(sld1, sld2) =>
          val (st1, vol1) = sMoveAux(endState, SMove(LLD(sld1.a, sld1.len)), nextBot)
          val newBot = st1.bots.find(_.bid == nextBot.bid).get
          val (st2, vol2) = sMoveAux(st1, SMove(LLD(sld2.a, sld2.len)), newBot)
          endState = st2.copy(energy = st2.energy + 2 * (math.abs(sld1.len) + 2 + math.abs(sld2.len)))
          endVolCoords = (vol1 ::: vol2).distinct ::: endVolCoords

        case Fill(nd) =>
          val cPrime = nextBot.pos + nd

          if (!endState.matrix.validateCoord(cPrime)) {
            throw CommandException("c' is not valid", nextCmd, endState, nextBot)
          }

          if (endState.matrix.get(cPrime) == Full) {
            println(s"WARNING: trying to fill $cPrime that is already filled")
            endState = endState.copy(energy = endState.energy + 6)
            endVolCoords = List(nextBot.pos, cPrime) ::: endVolCoords

          } else {
            endState = endState.copy(energy = endState.energy + 12, matrix = endState.matrix.fill(cPrime))
            endVolCoords = List(nextBot.pos, cPrime) ::: endVolCoords
          }

        case Fission(nd, m) =>
          val cPrime = nextBot.pos + nd

          if (nextBot.seeds.isEmpty) {
            throw CommandException("Seeds is empty", nextCmd, endState, nextBot)
          }
          if (!endState.matrix.validateCoord(cPrime)) {
            throw CommandException("c' is not valid", nextCmd, endState, nextBot)
          }
          if (endState.matrix.get(cPrime) == Full) {
            throw CommandException(s"$cPrime is full", nextCmd, endState, nextBot)
          }
          if (nextBot.seeds.size < m) {
            throw CommandException(s"Invalid m", nextCmd, endState, nextBot)
          }
          val updatedBot = nextBot.copy(seeds = nextBot.seeds.drop(m))
          val clonedBotSeeds = nextBot.seeds.slice(0, m)
          val clonedBot = Bot(clonedBotSeeds.head, cPrime, clonedBotSeeds.drop(1))
          endState = endState.copy(energy = endState.energy + 24, bots = endState.bots - nextBot + updatedBot + clonedBot)
          endVolCoords = List(nextBot.pos, cPrime) ::: endVolCoords

        case FusionP(pnd) =>
          val cPrime = nextBot.pos + pnd
          val fusionPair = botCmdPairs.find {
            case (b, FusionS(snd)) => b.pos == cPrime && b.pos + snd == nextBot.pos
            case _ => false
          }.getOrElse(throw CommandException(s"Can't find FusionP pair command", nextCmd, endState, nextBot))
          botCmdPairs -= fusionPair

          val (secBot, _) = fusionPair
          val primBot = nextBot

          val fusedBot = primBot.copy(seeds = primBot.seeds + secBot.bid ++ secBot.seeds)
          endState = endState.copy(energy = endState.energy - 24, bots = endState.bots - primBot - secBot + fusedBot)
          endVolCoords = List(primBot.pos, secBot.pos) ::: endVolCoords

        case FusionS(snd) =>
          val cPrime = nextBot.pos + snd
          val fusionPair = botCmdPairs.find {
            case (b, FusionP(pnd)) => b.pos == cPrime && b.pos + pnd == nextBot.pos
            case _ => false
          }.getOrElse(throw CommandException(s"Can't find FusionS pair command", nextCmd, endState, nextBot))
          botCmdPairs -= fusionPair

          val (primBot, _) = fusionPair
          val secBot = nextBot

          val fusedBot = primBot.copy(seeds = primBot.seeds + secBot.bid ++ secBot.seeds)
          endState = endState.copy(energy = endState.energy - 24, bots = endState.bots - primBot - secBot + fusedBot)
          endVolCoords = List(primBot.pos, secBot.pos) ::: endVolCoords

        case _: GFill => //FIXME: lots of repeated code...
          // put it back
          nextPair +=: botCmdPairs

          val gFills = botCmdPairs.filter { _._2.isInstanceOf[GFill] }
          if (gFills.size % 2 != 0) throw CommandException(s"GFill commands cannot be paired1", nextCmd, endState, nextBot)
          val groupings = (for {
            (b1, g1 @ GFill(nd1, fd1)) <- gFills
            (b2, g2 @ GFill(nd2, fd2)) <- gFills
            // they must point to each other
            if ((b1.pos + nd1) + fd1) == (b2.pos + nd2) && (b2.pos + nd2) + fd2 == (b1.pos + nd1)
          } yield Set((b1, g1), (b2, g2))).distinct.map(_.toList).toList

          if (groupings.isEmpty || !Set(1, 2, 4).contains(groupings.size))
            throw CommandException(s"GFill commands cannot be paired", nextCmd, endState, nextBot)

          // remove processed commands from the list so they are not picked again
          groupings.flatten.foreach(t => botCmdPairs -= t)

          // fill a region
          (groupings.head: @unchecked) match {
            // any diagonal line works
            case (b1, GFill(nd1, _)) :: (b2, GFill(nd2, _)) :: _ =>
              val start = b1.pos + nd1
              val end = b2.pos + nd2
              val (m, energy, regionVolCoords) = fillRegionAux(endState.matrix, start, end, { (matrix, coord) =>
                val energy = if (matrix.get(coord) == Full) 6 else 12
                (matrix.fill(coord), energy)
              })
              endState = endState.copy(matrix = m, energy = endState.energy + energy)
              endVolCoords = regionVolCoords ::: groupings.flatMap(_.map(_._1.pos)) ::: endVolCoords
          }

        case _: GVoid =>
          // put it back
          nextPair +=: botCmdPairs

          val gVoids = botCmdPairs.filter { _._2.isInstanceOf[GVoid] }
          if (gVoids.size % 2 != 0) throw CommandException(s"GVoid commands cannot be paired1", nextCmd, endState, nextBot)
          val groupings = (for {
            (b1, g1 @ GVoid(nd1, fd1)) <- gVoids
            (b2, g2 @ GVoid(nd2, fd2)) <- gVoids
            // they must point to each other
            if ((b1.pos + nd1) + fd1) == (b2.pos + nd2) && (b2.pos + nd2) + fd2 == (b1.pos + nd1)
          } yield Set((b1, g1), (b2, g2))).distinct.map(_.toList).toList

          if (groupings.isEmpty || !Set(1, 2, 4).contains(groupings.size))
            throw CommandException(s"GFill commands cannot be paired", nextCmd, endState, nextBot)

          // remove processed commands from the list so they are not picked again
          groupings.flatten.foreach(t => botCmdPairs -= t)

          // fill a region
          (groupings.head: @unchecked) match {
            // any diagonal line works
            case (b1, GVoid(nd1, _)) :: (b2, GVoid(nd2, _)) :: _ =>
              val start = b1.pos + nd1
              val end = b2.pos + nd2
              val (m, energy, regionVolCoords) = fillRegionAux(endState.matrix, start, end, { (matrix, coord) =>
                val energy = if (matrix.get(coord) == Full) -12 else 3
                (matrix.fill(coord), energy)
              })
              endState = endState.copy(matrix = m, energy = endState.energy + energy)
              endVolCoords = regionVolCoords ::: groupings.flatMap(_.map(_._1.pos)) ::: endVolCoords
          }

      }
    }

    (endState, endVolCoords)
  }

  private[this] def initialState(model: Matrix, trace: List[Command]): State = {
    State(
      0,
      Low,
      Matrix(model.dimension),
      bots = SortedSet(Bot(1, Coord(0, 0, 0), SortedSet(2 to 40: _*))),
      trace)
  }
}
