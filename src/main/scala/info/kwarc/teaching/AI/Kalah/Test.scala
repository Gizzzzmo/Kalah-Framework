package info.kwarc.teaching.AI.Kalah

import info.kwarc.teaching.AI.Kalah.Agents.{HumanPlayer, RandomPlayer}
import info.kwarc.teaching.AI.Kalah.Interfaces.{Fancy, Terminal}
import info.kwarc.teaching.AI.Kalah.util._
object Test {
  def main(args: Array[String]): Unit = {

    val int = new Fancy.FancyInterface(12)

    new Game(new HumanPlayer("Hans"),new RandomPlayer("Hurtz"),int + Terminal)(6,4).play
  }
}
