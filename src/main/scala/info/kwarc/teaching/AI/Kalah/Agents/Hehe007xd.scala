package info.kwarc.teaching.AI.Kalah.Agents
import java.lang
import collection.JavaConverters._

import info.kwarc.teaching.AI.Kalah.Board


class Hehe007xd extends Agent{
  type BoardState = (List[Int], List[Int], Int, Int)
  override val name: String = "Hehe007xd"
  override val students: lang.Iterable[String] = List("Jonas Beyer", "Thomas Roethenbacher", "Daniel Seitz").asJava
  private var draw: Int = 0
  private var houses: Int = 0
  private var currentboard:Board = null

  /**
    * is called once at the start of a game.
    *
    * @param board     The [[Board]] used (states in particular the parameters)
    * @param playerOne is true iff this agent is playerOne in the current game
    */
  override def init(board: Board, playerOne: Boolean): Unit = {
    houses = board.houses
    draw = houses * board.initSeeds
    currentboard = board
  }

  /**
    * This method is called by [[info.kwarc.teaching.AI.Kalah.Game]] to request an action. Note that you have <b>at most</b> 5sec to
    * return an action; otherwise the thread is aborted and the timeoutMove variable (by default=1) is chosen.
    * So you can use the full 5 seconds to constantly revise that variable.
    */
  override def move: Int = {
    val (pl1, pl2, m1, m2) = currentboard.getState
    val state1 = pl1.asScala.toList:+m1
    val state2 = pl2.asScala.toList:+m2
    return 0
  }

  def nextState: (List[Int], List[Int]) => Int => ((List[Int], List[Int]), Boolean) = (state1, state2) => move => {
    val nextState = (state1++state2).toArray
    nextState(move) = 0
    var finalCell = move+state1(move)
    for(i <- move+1 to finalCell){
      nextState(i%(nextState.length-1)) += 1// + nextState(i%(nextState.length-1))
    }
    finalCell %= nextState.length-1
    if(finalCell < houses && nextState(finalCell) == 1){
      nextState(houses) += 1 + nextState(2*houses-finalCell)
      nextState(finalCell) = 0
      nextState(2*houses-finalCell) = 0
    }
    (nextState.toList.splitAt(houses+1), finalCell == houses)
  }

  def getMoves: List[Int] => List[Int] = state => {
    (0 to houses-1).toList.filter(state(_) != 0)
  }
}