package icfpc2018

import scala.collection.SortedSet

object Simulator {

  case class SimulatorException(msg: String, st: State)
    extends Exception(s"$msg\nState: $st")

  case class CommandException(msg: String, cmd: Command, st: State, bot: Bot)
    extends Exception(s"$msg\nCommand: $cmd\n, Bot: $bot\nState: $st")

  case class ConflictingVolatileCoordsException(msg: String, volCoords: List[Coord], st: State)
    extends Exception(s"$msg\nVolatile coords: $volCoords, State: $st")

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

  def nextTick(st: State, model: Matrix): State = {
    val harmonicsCost = if (st.harmonics == High) 30L else 3L
    val initialUpdSt = st.copy(energy = st.energy +
      harmonicsCost * model.dimension * model.dimension * model.dimension +
      20 * st.bots.size)

    val (newSt, allVolCoords) = initialUpdSt.bots.foldLeft((initialUpdSt, List.empty[Coord])) {
      case ((st, volCoords), bot) =>
        val (newSt, vol) = nextCmd(st.copy(trace = st.trace.tail), model, bot, st.trace.head)
        (newSt, vol ::: volCoords)
    }

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

  def nextCmd(st: State, model: Matrix, bot: Bot, cmd: Command): (State, List[Coord]) = cmd match {
    case Halt =>
      if (bot.pos != Coord(0, 0, 0) || st.bots.size != 1 || st.harmonics != Low) {
        throw CommandException("Invalid Halt", Halt, st, bot)
      }
      (st.copy(bots = SortedSet.empty[Bot]), Nil)

    case Wait =>
      (st, List(bot.pos))

    case Flip =>
      (st.copy(harmonics = st.harmonics.flipped), List(bot.pos))

    case SMove(lld) =>
      val cPrime = bot.pos + lld
      val thisVolCoords = bot.pos.rangeTo(lld)

      if (!st.matrix.validateCoord(cPrime)) {
        throw CommandException("c' is not valid", cmd, st, bot)
      }
      thisVolCoords.foreach { coord =>
        if (st.matrix.get(coord) == Full) {
          throw CommandException(s"$coord is full", cmd, st, bot)
        }
      }

      val newBot = bot.copy(pos = cPrime)
      (st.copy(energy = st.energy + 2 * math.abs(lld.len), bots = st.bots - bot + newBot), thisVolCoords)

    case LMove(sld1, sld2) =>
      val (st1, vol1) = nextCmd(st, model, bot, SMove(LLD(sld1.a, sld1.len)))
      val newBot = st1.bots.find(_.bid == bot.bid).get
      val (st2, vol2) = nextCmd(st1, model, newBot, SMove(LLD(sld2.a, sld2.len)))
      (st2.copy(energy = st2.energy + 4), (vol1 ::: vol2).distinct)

    case Fission(nd, m) =>
      val cPrime = bot.pos + nd

      if (bot.seeds.isEmpty) {
        throw CommandException("Seeds is empty", cmd, st, bot)
      }
      if (!st.matrix.validateCoord(cPrime)) {
        throw CommandException("c' is not valid", cmd, st, bot)
      }
      if (st.matrix.get(cPrime) == Full) {
        throw CommandException(s"$cPrime is full", cmd, st, bot)
      }
      if (bot.seeds.size < m + 1) {
        throw CommandException(s"Invalid m", cmd, st, bot)
      }
      val updatedBot = bot.copy(seeds = bot.seeds.drop(m + 1))
      val clonedBot = Bot(bot.seeds.head, cPrime, bot.seeds.slice(1, m))
      (st.copy(energy = st.energy + 24, bots = st.bots - bot + updatedBot + clonedBot), List(bot.pos, cPrime))

    case Fill(nd) =>
      val cPrime = bot.pos + nd

      if (!st.matrix.validateCoord(cPrime)) {
        throw CommandException("c' is not valid", cmd, st, bot)
      }

      if (st.matrix.get(cPrime) == Full) {
        println(s"WARNING: trying to fill $cPrime that is already filled")
        (st.copy(energy = st.energy + 6), List(bot.pos, cPrime))
      } else {
        (st.copy(energy = st.energy + 12, matrix = st.matrix.fill(cPrime)), List(bot.pos, cPrime))
      }

    case FusionP(nd) =>
      val cPrime = bot.pos + nd
      val secBot = st.bots.find(_.pos == cPrime).get
      val fusedBot = bot.copy(seeds = bot.seeds + secBot.bid ++ secBot.seeds)
      (st.copy(energy = st.energy - 24, bots = st.bots - bot - secBot + fusedBot), List(bot.pos, secBot.pos))

    case FusionS(nd) =>
      // this could be better, we are not catching certain errors
      val cPrime = bot.pos + nd
      if (!st.bots.exists(_.pos == cPrime)) {
        throw CommandException(s"Invalid primary bot", cmd, st, bot)
      }
      (st, Nil)
  }

  private[this] def initialState(model: Matrix, trace: List[Command]): State = {
    State(
      0,
      Low,
      Matrix(model.dimension),
      bots = SortedSet(Bot(1, Coord(0, 0, 0), SortedSet(2 to 20: _*))),
      trace)
  }
}
