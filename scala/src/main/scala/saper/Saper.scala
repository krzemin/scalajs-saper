package saper

import scala.annotation.tailrec
import scala.util.Random

object Saper {

  case class GameConfig(width: Int, height: Int, mines: Int)

  trait Field { def str: String }

  case object Empty extends Field { def str = "_" }
  case object Mine extends Field { def str = "*" }
  case class Number(n: Int) extends Field { def str = n.toString }

  trait FieldVisibility
  case object Closed extends FieldVisibility
  case object Opened extends FieldVisibility
  case object Flagged extends FieldVisibility

  trait GameState
  case object GameInProgress extends GameState
  case object Fail extends GameState
  case class Win(moves: Int) extends GameState
  
  case class Game(config: GameConfig) {

    var initialized = false

    var board: Map[(Int, Int), Field] = Map.empty

    var visibilities: Map[(Int, Int), FieldVisibility] =
      (for(x <- 0 until config.width;
           y <- 0 until config.height)
        yield (x, y) -> Closed).toMap

    var state: GameState = GameInProgress

    var moves: Int = 0

    def reset(): Unit = {
      clearFields()
      initialized = false
      moves = 0
      state = GameInProgress
    }

    clearFields()

    def click(x: Int, y: Int): Unit = {
      checkInitialized(x, y)

      if(state == GameInProgress && visibilities((x, y)) == Closed) {
        moves += 1
        board((x, y)) match {
          case Empty =>
            openNeighEmpties(x, y)
          case Mine =>
            for(x <- 0 until config.width;
                y <- 0 until config.height) {
              visibilities += (x, y) -> Opened
            }
            state = Fail
          case Number(_) =>
            visibilities += (x, y) -> Opened
        }
        checkIfWin()
      }
    }

    def markFlag(x: Int, y: Int): Unit = {
      if(initialized && state == GameInProgress) {
        visibilities((x, y)) match {
          case Closed =>
            visibilities += (x, y) -> Flagged
          case Flagged =>
            visibilities += (x, y) -> Closed
          case Opened =>
        }
        checkIfWin()
      }
    }

    def bothClick(x: Int, y: Int): Unit = {
      if(initialized && state == GameInProgress && visibilities((x, y)) == Opened) {
        board((x, y)) match {
          case Number(n) =>
            val neighCoords = for {
              px <- x - 1 to x + 1
              py <- y - 1 to y + 1
              if (px, py) != (x, y)
              if (0 until config.width) contains px
              if (0 until config.height) contains py
            } yield (px, py)

            val neighVisibilities = neighCoords.map(f => visibilities(f))
            if(neighVisibilities.count(_ == Flagged) == n) {
              neighCoords.foreach { case (x, y) =>
                if(visibilities((x, y)) == Closed) {
                  click(x, y)
                }
              }
            }
          case _ =>
        }
      }
    }

    def openNeighEmpties(x: Int, y: Int): Unit = {
      if(visibilities(x, y) != Opened) {
        visibilities += (x, y) -> Opened
        if (board(x, y) == Empty) {
          for {
            dx <- -1 to 1
            dy <- -1 to 1
            if (dx, dy) != (0, 0)
            (nx, ny) = (x + dx, y + dy)
            if (0 until config.width) contains nx
            if (0 until config.height) contains ny
          } {
            openNeighEmpties(nx, ny)
          }
        }
      }
    }

    def checkIfWin(): Unit = {
      val flagCondition: Boolean = (for {
        x <- 0 until config.width
        y <- 0 until config.height
      } yield !((board((x, y)) == Mine) ^ (visibilities((x, y)) == Flagged)))
        .forall(x => x)

      lazy val minesCoords = board.filter(_._2 == Mine).keySet
      lazy val closedCoords = visibilities.filter(_._2 == Closed).keySet
      lazy val openedFieldsCondition: Boolean = minesCoords == closedCoords

      if(flagCondition || openedFieldsCondition) {
        state = Win(moves)
      }
    }

    override def toString: String = {
      (0 until config.height).map { y =>
        (0 until config.width).map { x =>
          visibilities((x, y)) match {
            case Opened => board((x, y)).str
            case Flagged => "#"
            case Closed => " "
          }
        }.mkString
      }.mkString("\n")
    }

    private lazy val rand = new Random()

    def checkInitialized(x: Int, y: Int): Unit = {
      if(!initialized) {
        initialized = true
        do {
          clearFields()
          placeMines(config.mines)
        } while (board((x, y)) != Empty)
      }
    }

    def clearFields(): Unit = {
      (0 until config.width).foreach { x =>
        (0 until config.height).foreach { y =>
          board += (x, y) -> Empty
          visibilities += (x, y) -> Closed
        }
      }
    }

    @tailrec
    private def placeMines(minesLeft: Int): Unit = minesLeft match {
      case 0 => ()
      case k =>
        val x = rand.nextInt(config.width)
        val y = rand.nextInt(config.height)
        if(board((x,y)) != Mine) {
          board += (x, y) -> Mine
          placeNumbers(x, y)
          placeMines(k - 1)
        } else {
          placeMines(k)
        }
    }

    private def placeNumbers(x: Int, y: Int): Unit = {
      for {
        px <- x - 1 to x + 1
        py <- y - 1 to y + 1
        if (px, py) != (x, y)
        if (0 until config.width) contains px
        if (0 until config.height) contains py
      } {
        board((px, py)) match {
          case Empty =>
            board += (px, py) -> Number(1)
          case Number(k) =>
            board += (px, py) -> Number(k + 1)
          case _ =>
        }
      }
    }
  }

}
