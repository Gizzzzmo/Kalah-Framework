package info.kwarc.teaching.AI.Kalah.Agents

import info.kwarc.teaching.AI.Kalah.{Board, Game}

import scala.collection.JavaConverters._
import scala.util.Random

/**
  * The abstract class for agents. Needs to implement four things:
  *
  * - [[Agent.name]]     : What you call your agent (just for fun). Please make this one fixed (unlike in the example
  *                   below)
  * - [[Agent.students]] : Please put your full names in this list.
  * - [[Agent.init]]     : Called at the start of a game.
  * - [[Agent.move]]     : is called when it's the agent's turn
  */
abstract class Agent {
  val name : String
  val students : java.lang.Iterable[String]
  implicit val me = this
  /**
    * is called once at the start of a game.
    * @param board The [[Board]] used (states in particular the parameters)
    * @param playerOne is true iff this agent is playerOne in the current game
    */
  def init(board : Board, playerOne : Boolean)

  /**
    * This method is called by [[Game]] to request an action. Note that you have <b>at most</b> 5sec to
    * return an action; otherwise the thread is aborted and the timeoutMove variable (by default=1) is chosen.
    * So you can use the full 5 seconds to constantly revise that variable.
    */
  def move : Int

  /**
    * This variable is read if your Agent times out and didn't return a value. It is explicitly made mutable, so
    * your move-method can regularly update it.
    */
  @volatile var timeoutMove : Int = 1
}

/**
  * A human player that reads actions (as field numbers) from stdin. Can be used to test your agents.
  * @param name     : What you call your agent (just for fun).
  */
final case class HumanPlayer(val name : String) extends Agent {
  val students = List("Dennis").asJava
  private var currentboard : Board = null
  private var playerone = None.asInstanceOf[Option[Boolean]]
  private var houses = 0

  def init(board : Board, playerOne : Boolean): Unit = {
    currentboard = board
    houses = currentboard.houses
    playerone = Some(playerOne)
    println(board.getMoves)
    println("Initializing for " + name)
    println("Playing Kalah(" + board.houses + "," + board.initSeeds + ")")
  }

  def move : Int = {
    println("Your move, " + name)
    println(currentboard.asString(this))
    val (pl1, pl2, m1, m2) = currentboard.getState
    val state1 = pl1.asScala.toList:+m1
    val state2 = pl2.asScala.toList:+m2
    println(getMoves)
    print("Enter house: ")
    val move = scala.io.StdIn.readInt
    val ((nextState1, nextState2), again) = nextState(state1, state2)(move-1)
    println(nextState2)
    println(nextState1)
    println(again)
    move
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

/**
  * A player that always moves randomly
  * @param name : What you call your agent (just for fun)
  */
class RandomPlayer(val name : String) extends Agent {
  val students = List("Dennis").asJava
  private var currentboard : Board = null

  def init(board : Board, playerOne : Boolean): Unit = {
    currentboard = board
  }
  def move : Int = {
    val ls = currentboard.getHouses
    val rnd = new Random
    var i = rnd.nextInt(ls.asScala.size)
    while (ls.asScala.toList(i) == 0) {
      i = rnd.nextInt(ls.asScala.size)
    }
    timeoutMove = i + 1
    i + 1
  }
}

/**
  * A player that always times out. For testing purposes
  */
class TimeOut extends Agent {
  val name = "Timeouter"
  val students = List("Dennis").asJava
  private var currentboard : Board = null

  def init(board : Board, playerOne : Boolean): Unit = {
    currentboard = board
    Thread.sleep(11000)
    println("Woke up!")
  }
  def move : Int = {
    Thread.sleep(8000)
    println("Woke up!")
    0
  }
}