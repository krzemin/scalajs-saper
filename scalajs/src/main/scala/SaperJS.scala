import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom.{HTMLElement, HTMLAnchorElement, HTMLButtonElement, document}

import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import saper.Saper._

object SaperJS extends JSApp {

  val config = GameConfig(5, 5, 5)
  val game = Game(config)

  def fieldByState(f: Field, v: FieldVisibility, x: Int, y: Int): HTMLAnchorElement = {
    val fieldId = s"field-$x-$y"
    val fieldCls = "btn btn-primary"

    (v match {
      case Closed =>
        a(id := fieldId, cls := fieldCls)("?")
      case Opened =>
        f match {
          case Empty =>
            a(id := fieldId, cls := fieldCls)(".")
          case Mine =>
            a(id := fieldId, cls := "btn btn-danger")("*")
          case Number(n) =>
            a(id := fieldId, cls := "btn btn-success")(n)
        }
      case Flagged =>
        a(id := fieldId, cls := "btn btn-info")("#")
    }).render
  }

  def leftClick(x: Int, y: Int): Unit = {
    game.click(x, y)
    renderGame(gameDiv)
  }

  def rightClick(x: Int, y: Int): Unit = {
    game.markFlag(x, y)
    renderGame(gameDiv)
  }

  def renderGame(gameDiv: HTMLElement) = {
    val rows = (0 until config.height).map { y =>
      val buttons = (0 until config.width).map { x =>
        val link = fieldByState(game.board((x, y)), game.visibilities((x, y)), x, y)
        link.onclick = (e: dom.MouseEvent) => {
          leftClick(x, y)
        }
        link.oncontextmenu = (e: dom.MouseEvent) => {
          rightClick(x, y)
        }
        link
      }
      div(buttons)
    }

    gameDiv.innerHTML = ""
    gameDiv.appendChild(div(rows).render)

    gameDiv.appendChild(p(game.state.toString).render)

    gameDiv.appendChild(p(s"Moves: ${game.moves}").render)
  }


  lazy val gameDiv = dom.document.getElementById("game")

  def main(): Unit = {
    renderGame(gameDiv)
  }

}
