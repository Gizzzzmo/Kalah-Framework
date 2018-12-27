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
    val state1 = pl1.asScala.toArray:+m1
    val state2 = pl2.asScala.toArray:+m2
    return 0
  }

  def nextState: (Array[Int], Array[Int], Boolean) => Int => ((Array[Int], Array[Int]), Boolean, Boolean) = (state1, state2, player1) => move => {
    val nextState = get(state1, state2)
    val nextStateSet = set(state1, state2)
    val nextStateAdd = add(state1, state2)
    val length = state1.length + state2.length
    nextStateSet(move)(0)
    var finalCell = move+state1(move)
    for(i <- move+1 to finalCell){
      nextStateAdd(i%(length-1))(1)// + nextState(i%(nextState.length-1))
    }
    finalCell %= length-1
    if(finalCell < houses && nextState(finalCell) == 1){
      nextStateAdd(houses)(1 + nextState(2*houses-finalCell))
      nextStateSet(finalCell)(0)
      nextStateSet(2*houses-finalCell)(0)
    }
    ((state1, state2), (finalCell != houses)^player1, finalCell == houses)
  }

  def utility: State => Double = state => {
    val x = state.state._1
    val y = state.state._2

    if(x.last == y.last == draw) .5f
    else if(x.last > draw)1
    else if(y.last > draw)1
    else {
      val xdiff = draw+1-x(houses)
      val ydiff = draw+1-y(houses)
      val visit = Math.log(state.visitCount)
      state.winScore * ydiff/((ydiff+xdiff) * (if(visit == 0) .6 else visit))
    }

  }

  def get: (Array[Int], Array[Int]) => Int => Int = (x, y) => i => {
    if(i < x.length)x(i)
    else y(i-x.length)
  }

  def set: (Array[Int], Array[Int]) => Int => Int => Unit = (x, y) => i => z=> {
    if(i < x.length)x(i) = z
    else y(i-x.length) = z
  }

  def add: (Array[Int], Array[Int]) => Int => Int => Unit = (x, y) => i => z=> {
    if(i < x.length)x(i) += z
    else y(i-x.length) += z
  }

  def getMoves: Array[Int] => List[Int] = state => {
    (0 to houses-1).toList.filter(state(_) != 0)
  }

  def getNextStates: State => List[State] = state => {
    val x = state.state
    val boards = getMoves(state.state._1).map(nextState(x._1, x._2, state.player1)(_))
    boards.map(boardToState)
  }

  def boardToState: (((Array[Int], Array[Int]), Boolean, Boolean)) => State = state => {
    val board = state._1
    val player = state._2
    val repeat = state._3
    if(repeat)State(0, 1, board, player) else State(0, 1, (board._2, board._1), player)
  }


  case class Node(parent: Node, state: State, var children: List[Node])
  case class Tree(root: Node)
  case class State(var winScore: Double, var visitCount: Int, state: (Array[Int], Array[Int]), player1: Boolean)

  def bestNextNode: Node => Node = node => {
    node.children.maxBy(n => utility(n.state))
  }

  def expandNode: Node => Unit = node => {
    node.children = getNextStates(node.state).map(state => Node(node, state, null))
  }
}
