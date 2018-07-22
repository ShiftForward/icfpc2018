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
    println()
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

    println("Old Bots:\n" + initialUpdSt.bots.map(b => (b.bid, b.pos)).mkString("\n"))
    println("Commands:\n" + cmdSet.mkString("\n"))
    val (newSt, allVolCoords) = processCmds(initialUpdSt, model, botSet, cmdSet)
    println("New Bots:\n" + newSt.bots.map(b => (b.bid, b.pos)).mkString("\n"))
    println()

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

    val botCmdPairs = mutable.ListBuffer(currentBotSet.zip(currentCmdSet).toArray: _*)
    var endState = initState
    var endVolCoords = List.empty[Coord]

    while (botCmdPairs.nonEmpty) {
      val next: (Bot, Command) = botCmdPairs.head
      botCmdPairs -= next
      val (nextBot: Bot, nextCmd) = next

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
          println(s"Adding vols (${nextBot.bid}) : " + newVolCoords)
          endVolCoords = endVolCoords ::: newVolCoords

        //          val cPrime = bot.pos + lld
        //      val thisVolCoords = bot.pos.rangeTo(lld)
        //
        //      if (!st.matrix.validateCoord(cPrime)) {
        //        throw CommandException("c' is not valid", cmd, st, bot)
        //      }
        //      thisVolCoords.foreach { coord =>
        //        if (st.matrix.get(coord) == Full) {
        //          throw CommandException(s"$coord is full", cmd, st, bot)
        //        }
        //      }
        //
        //      val newBot = bot.copy(pos = cPrime)
        //      (st.copy(energy = st.energy + 2 * math.abs(lld.len), bots = st.bots - bot + newBot), thisVolCoords)

        case LMove(sld1, sld2) =>
          val (st1, vol1) = sMoveAux(endState, SMove(LLD(sld1.a, sld1.len)), nextBot)
          val newBot = st1.bots.find(_.bid == nextBot.bid).get
          val (st2, vol2) = sMoveAux(st1, SMove(LLD(sld2.a, sld2.len)), newBot)
          endState = st2.copy(energy = st2.energy + 2 * (math.abs(sld1.len) + 2 + math.abs(sld2.len)))
          endVolCoords = (vol1 ::: vol2).distinct ::: endVolCoords

        //          val (st1, vol1) = nextCmd(st, model, bot, SMove(LLD(sld1.a, sld1.len)))
        //      val newBot = st1.bots.find(_.bid == bot.bid).get
        //      val (st2, vol2) = nextCmd(st1, model, newBot, SMove(LLD(sld2.a, sld2.len)))
        //      (st2.copy(energy = st2.energy + 4), (vol1 ::: vol2).distinct)

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
            println(s"Adding vols (${nextBot.bid}) : " + List(nextBot.pos, cPrime))
            endVolCoords = List(nextBot.pos, cPrime) ::: endVolCoords
          }

        case Fission(nd, m) =>
          println("In the fission")
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
          println("Original bot: " + nextBot)
          println("Cloned bot: " + clonedBot)
          endState = endState.copy(energy = endState.energy + 24, bots = endState.bots - nextBot + updatedBot + clonedBot)
          println(endState.bots.map(b => (b.bid, b.pos)))
          endVolCoords = List(nextBot.pos, cPrime) ::: endVolCoords

        case FusionP(pnd) =>
          val cPrime = nextBot.pos + pnd
          val fusionPair = botCmdPairs.find {
            case (b, FusionS(snd)) => b.pos == cPrime && b.pos + snd == nextBot.pos
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
          }.getOrElse(throw CommandException(s"Can't find FusionS pair command", nextCmd, endState, nextBot))
          botCmdPairs -= fusionPair

          val (primBot, _) = fusionPair
          val secBot = nextBot

          val fusedBot = primBot.copy(seeds = primBot.seeds + secBot.bid ++ secBot.seeds)
          endState = endState.copy(energy = endState.energy - 24, bots = endState.bots - primBot - secBot + fusedBot)
          endVolCoords = List(primBot.pos, secBot.pos) ::: endVolCoords
      }
    }

    (endState, endVolCoords)
  }

  //  def nextCmd(st: State, model: Matrix, bot: Bot, cmd: Command): (State, List[Coord]) = cmd match {
  //    case Halt =>
  //      if (bot.pos != Coord(0, 0, 0) || st.bots.size != 1 || st.harmonics != Low) {
  //        throw CommandException("Invalid Halt", Halt, st, bot)
  //      }
  //      (st.copy(bots = SortedSet.empty[Bot]), Nil)
  //
  //    case Wait =>
  //      (st, List(bot.pos))
  //
  //    case Flip =>
  //      (st.copy(harmonics = st.harmonics.flipped), List(bot.pos))
  //
  //    case SMove(lld) =>
  //      val cPrime = bot.pos + lld
  //      val thisVolCoords = bot.pos.rangeTo(lld)
  //
  //      if (!st.matrix.validateCoord(cPrime)) {
  //        throw CommandException("c' is not valid", cmd, st, bot)
  //      }
  //      thisVolCoords.foreach { coord =>
  //        if (st.matrix.get(coord) == Full) {
  //          throw CommandException(s"$coord is full", cmd, st, bot)
  //        }
  //      }
  //
  //      val newBot = bot.copy(pos = cPrime)
  //      (st.copy(energy = st.energy + 2 * math.abs(lld.len), bots = st.bots - bot + newBot), thisVolCoords)
  //
  //    case LMove(sld1, sld2) =>
  //      val (st1, vol1) = nextCmd(st, model, bot, SMove(LLD(sld1.a, sld1.len)))
  //      val newBot = st1.bots.find(_.bid == bot.bid).get
  //      val (st2, vol2) = nextCmd(st1, model, newBot, SMove(LLD(sld2.a, sld2.len)))
  //      (st2.copy(energy = st2.energy + 4), (vol1 ::: vol2).distinct)
  //
  //    case Fission(nd, m) =>
  //      val cPrime = bot.pos + nd
  //
  //      if (bot.seeds.isEmpty) {
  //        throw CommandException("Seeds is empty", cmd, st, bot)
  //      }
  //      if (!st.matrix.validateCoord(cPrime)) {
  //        throw CommandException("c' is not valid", cmd, st, bot)
  //      }
  //      if (st.matrix.get(cPrime) == Full) {
  //        throw CommandException(s"$cPrime is full", cmd, st, bot)
  //      }
  //      if (bot.seeds.size < m + 1) {
  //        throw CommandException(s"Invalid m", cmd, st, bot)
  //      }
  //      val updatedBot = bot.copy(seeds = bot.seeds.drop(m + 1))
  //      val clonedBot = Bot(bot.seeds.head, cPrime, bot.seeds.slice(1, m))
  //      (st.copy(energy = st.energy + 24, bots = st.bots - bot + updatedBot + clonedBot), List(bot.pos, cPrime))
  //
  //    case Fill(nd) =>
  //      val cPrime = bot.pos + nd
  //
  //      if (!st.matrix.validateCoord(cPrime)) {
  //        throw CommandException("c' is not valid", cmd, st, bot)
  //      }
  //
  //      if (st.matrix.get(cPrime) == Full) {
  //        println(s"WARNING: trying to fill $cPrime that is already filled")
  //        (st.copy(energy = st.energy + 6), List(bot.pos, cPrime))
  //      } else {
  //        (st.copy(energy = st.energy + 12, matrix = st.matrix.fill(cPrime)), List(bot.pos, cPrime))
  //      }
  //
  //    case FusionP(nd) =>
  //      val cPrime = bot.pos + nd
  //      val secBot = st.bots.find(_.pos == cPrime).get
  //      val fusedBot = bot.copy(seeds = bot.seeds + secBot.bid ++ secBot.seeds)
  //      (st.copy(energy = st.energy - 24, bots = st.bots - bot - secBot + fusedBot), List(bot.pos, secBot.pos))
  //
  //    case FusionS(nd) =>
  //      // this could be better, we are not catching certain errors
  //      val cPrime = bot.pos + nd
  //      if (!st.bots.exists(_.pos == cPrime)) {
  //        throw CommandException(s"Invalid primary bot", cmd, st, bot)
  //      }
  //      (st, Nil)
  //  }

  private[this] def initialState(model: Matrix, trace: List[Command]): State = {
    State(
      0,
      Low,
      Matrix(model.dimension),
      bots = SortedSet(Bot(1, Coord(0, 0, 0), SortedSet(2 to 40: _*))),
      trace)
  }
}
