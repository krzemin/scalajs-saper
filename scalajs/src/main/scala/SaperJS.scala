import scala.scalajs.js.JSApp
import org.scalajs.dom._

import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import saper.Saper._

object SaperJS extends JSApp {

  var config = GameConfig(10, 10, 10)
  var game = Game(config)

  def fieldByState(f: Field, v: FieldVisibility, x: Int, y: Int): HTMLAnchorElement = {
    val fieldId = s"field-$x-$y"
    (v match {
      case Closed =>
        a(id := fieldId, cls := "btn btn-primary")("?")
      case Opened =>
        f match {
          case Empty =>
            a(id := fieldId, cls := "btn btn-default")(".")
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
    renderGame()
  }

  def rightClick(x: Int, y: Int): Unit = {
    game.markFlag(x, y)
    renderGame()
  }

  def bothClick(x: Int, y: Int): Unit = {
    game.bothClick(x, y)
    renderGame()
  }


  def renderGame(): Unit = {
    val rows = (0 until config.height).map { y =>
      val buttons = (0 until config.width).map { x =>
        val link = fieldByState(game.board((x, y)), game.visibilities((x, y)), x, y)
        link.onclick = (e: MouseEvent) => {
          if(e.button == 0)
            leftClick(x, y)
          else
            bothClick(x, y)
        }
        link.oncontextmenu = (e: MouseEvent) => {
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

  def renderMenu(): Unit = {
    menuDiv.innerHTML =
      """
        |<h1>Saper</h1>
        |
        |<div class="dropdown">
        |  <button class="btn btn-primary" type="button" id="resetGame">Od nowa</button>
        |  <button class="btn btn-primary dropdown-toggle" type="button" id="levels" data-toggle="dropdown" aria-expanded="true">
        |    Wybierz poziom trudności
        |    <span class="caret"></span>
        |  </button>
        |  <ul class="dropdown-menu" role="menu" aria-labelledby="levels">
        |    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" id="lev1">Początkujący</a></li>
        |    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" id="lev2">Zaawansowany</a></li>
        |    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" id="lev3">Ekspert</a></li>
        |  </ul>
        |</div>
        |<br />
      """.stripMargin

    document.getElementById("resetGame").onclick = (e: MouseEvent) => {
      game.reset()
      renderGame()
    }

    document.getElementById("lev1").onclick = (e: MouseEvent) => {
      config = GameConfig(10, 10, 10)
      game = new Game(config)
      renderGame()
    }

    document.getElementById("lev2").onclick = (e: MouseEvent) => {
      config = GameConfig(15, 15, 40)
      game = new Game(config)
      renderGame()
    }
    document.getElementById("lev3").onclick = (e: MouseEvent) => {
      config = GameConfig(25, 15, 99)
      game = new Game(config)
      renderGame()
    }
  }


  lazy val gameDiv = document.getElementById("game")
  lazy val menuDiv = document.getElementById("menu")

  def main(): Unit = {
    renderMenu()
    renderGame()
  }

}
