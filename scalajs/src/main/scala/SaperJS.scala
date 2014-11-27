import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom.{HTMLButtonElement, document}

import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import saper._

object SaperJS extends JSApp {

  //val game


  def main(): Unit = {

    //dom.document.getElementById("game").innerHTML = "<h1>DUPA</h1>"

    val config = Saper.GameConfig(10, 10, 10)
    val board = Saper.Board(config)


    println(board)


  }

}
