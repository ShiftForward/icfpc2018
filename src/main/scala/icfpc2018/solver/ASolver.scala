package icfpc2018.solver

import scala.collection.{ SortedSet, mutable }

import icfpc2018._
import icfpc2018.solver.pathing.AStarPathFinder

class ASolver(model: Matrix, nseeds: Int) {
  import ASolver._

  lazy val botSplits: Map[Int, List[Action]] = {
    val voxels = mutable.Set(model.voxels.toList: _*)

    val dx = 5
    val dz = 4

    val sx = voxels.toList.sortBy(_.x).grouped((voxels.size + dx - 1) / dx).toList
    val sz = voxels.toList.sortBy(_.z).grouped((voxels.size + dz - 1) / dz).toList
    val target = mutable.Map[Int, List[Action]]()

    var bid = 1
    (0 until dx).foreach { x =>
      (0 until dz).foreach { z =>
        val coords = voxels.filter(c => c.x <= sx(x).last.x && c.z <= sz(z).last.z)
        voxels --= coords
        // FIXME sort this better
        target(bid) = coords.toList.sortBy(_.y).map(Paint)
        bid += 1
      }
    }

    target.toMap
  }

  def getActions(bid: Int): mutable.Queue[Action] = {
    mutable.Queue[Action](botSplits.getOrElse(bid, Nil): _*)
  }

  val volatileCoords = mutable.Set[Coord]()
  val commands = mutable.ListBuffer[Command]()

  def addCommand(coord: Coord, command: Command): Boolean = {
    val vc = (command.volatileCoords(coord) - coord)
    if (volatileCoords.intersect(vc).nonEmpty) {
      volatileCoords += coord
      commands += Wait
      false
    } else {
      volatileCoords ++= vc
      commands += command
      true
    }
  }

  def solve(): List[Command] = {
    commands.clear()
    val botState = mutable.ListBuffer[BotState](BotState(Bot(1, Coord(0, 0, 0), SortedSet((2 to nseeds): _*)), Coord(0, 0, 0), mutable.Queue(DoDivide), false))
    var state: GlobalState = Divide
    var currentModel = Matrix(model.dimension)
    var flipped = false

    while (state != Done) {
      volatileCoords.clear()
      botState.foreach { s =>
        volatileCoords += s.bot.pos
      }

      val doNegativeFlip = (flipped && currentModel.isGrounded && !botState.exists(_.requiresHarmonics))

      val pf = new AStarPathFinder(currentModel)

      botState.toList.foreach { s =>
        val b = s.bot
        if (b.bid == 1 && doNegativeFlip) {
          if (addCommand(b.pos, Flip))
            flipped = false
        } else if (s.actions.nonEmpty) {
          val action = s.actions.front

          action match {
            case DoDivide =>
              val nd = if (b.pos.x < model.dimension - 1) NCD(1, 0, 0) else NCD(0, 0, 1)
              if (addCommand(s.bot.pos, Fission(nd, b.seeds.size - 1)))
                s.actions.dequeue()
              val nextBot = BotState(Bot(b.seeds.head, b.pos + nd, b.seeds.tail), b.pos + nd, mutable.Queue[Action](), false)
              if (nextBot.bot.seeds.nonEmpty)
                nextBot.actions += DoDivide
              botState += nextBot
              s.bot = b.copy(seeds = SortedSet.empty[Int])

            case Paint(coord) =>
              val coordToMove = coord.copy(y = coord.y + 1)

              if (b.pos == coordToMove) {
                if (!currentModel.supported(coord) && !flipped) {
                  s.requiresHarmonics = true
                  if (addCommand(b.pos, Flip))
                    flipped = true
                } else if (addCommand(s.bot.pos, Fill(NCD(0, -1, 0)))) {
                  s.actions.dequeue()
                  currentModel = currentModel.fill(coord)
                  s.requiresHarmonics = false
                }
              } else {
                val path = pf.findPath(b.pos, coordToMove)
                if (addCommand(s.bot.pos, path.head)) {
                  path.head match {
                    case SMove(lld) =>
                      s.bot = b.copy(pos = b.pos + lld)
                    case LMove(sld1, sld2) =>
                      s.bot = b.copy(pos = b.pos + sld1 + sld2)
                    case _ => //
                  }
                }
              }

            case Delete(coord) => // todo

            case GoTo(coord) =>
              if (s.bot.pos == coord)
                addCommand(s.bot.pos, Wait)
              else {
                val path = pf.findPath(s.bot.pos, coord)
                if (addCommand(s.bot.pos, path.head)) {
                  path.head match {
                    case SMove(lld) =>
                      s.bot = s.bot.copy(pos = s.bot.pos + lld)
                    case LMove(sld1, sld2) =>
                      s.bot = s.bot.copy(pos = s.bot.pos + sld1 + sld2)
                    case _ => //
                  }
                  if (s.bot.pos == coord)
                    s.actions.dequeue()
                }
              }

            case DoJoin =>
              if (s.bot.bid == botState.size) {
                // FIXME hardcoded for x
                val nd = NCD(-1, 0, 0)
                if (botState(s.bot.bid - 2).bot.pos == s.bot.pos + nd) {
                  if (addCommand(s.bot.pos, FusionS(nd))) {
                    botState.remove(botState.size - 1)
                  }
                } else
                  addCommand(s.bot.pos, Wait)
              } else if (s.bot.bid == botState.size - 1) {
                val nd = NCD(1, 0, 0)
                if (botState(s.bot.bid).bot.pos == s.bot.pos + nd) {
                  addCommand(s.bot.pos, FusionP(nd))
                  if (s.bot.bid == 1)
                    s.actions.dequeue()
                } else
                  addCommand(s.bot.pos, Wait)
              } else
                addCommand(s.bot.pos, Wait)
          }
        } else {
          addCommand(s.bot.pos, Wait)
        }
      }

      if (botState.forall(_.actions.isEmpty)) {
        state = state.next
        if (state == DoWork) {
          botState.foreach { s =>
            s.actions ++= getActions(s.bot.bid)
          }
        } else if (state == Join) {
          botState.foreach { s =>
            s.actions.enqueue(GoTo(s.initialPos))
            s.actions.enqueue(DoJoin)
          }
        }
      }
    }

    if (flipped)
      commands += Flip

    commands += Halt
    commands.toList
  }
}

/**
 * Only works for assembly problems (for now).
 */
object ASolver extends Solver {
  case class BotState(var bot: Bot, initialPos: Coord, actions: mutable.Queue[Action], var requiresHarmonics: Boolean)

  sealed trait GlobalState {
    def next: GlobalState
  }

  case object Divide extends GlobalState {
    val next = DoWork
  }

  case object DoWork extends GlobalState {
    val next = Return
  }

  case object Return extends GlobalState {
    val next = Join
  }

  case object Join extends GlobalState {
    val next = Done
  }

  case object Done extends GlobalState {
    val next = Done
  }

  sealed trait Action
  case object DoDivide extends Action
  case class Paint(coord: Coord) extends Action
  case class Delete(coord: Coord) extends Action
  case class GoTo(coord: Coord) extends Action
  case object DoJoin extends Action

  def solve(model: Matrix): List[Command] = {
    new ASolver(model, 20).solve()
  }
}
