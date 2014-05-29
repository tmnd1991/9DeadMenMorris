package main


import main.model.{MyPhase, StateGenerator, MyState}
import main.model.Heuristic.{ConcreteHeuristic, Heuristic}
import main.model.Tree.Node
import main.model.Moves.{Move, NoMove}


/**
 * Created by tmnd on 24/05/14.
 */
object MyMain {
  def main(args : Array[String]) = {
    var n = new Node(new MyState(true,NoMove),new ConcreteHeuristic())
    val board = Map( true->List("A7","G7","C5","E5","E4","C3"), false->List("D6","F6","C3","B2","B4","C4"))
    var i = 0
    println(n.data)
    var lastEatenT = n.data.eaten(true)
    var lastEatenF = n.data.eaten(false)
    while(!n.data.hasWon(n.data.toMove)){
      println(i)
      val start = System.currentTimeMillis
      n = nextMove(n)._1
      val end = System.currentTimeMillis()
      if (end-start>60000){
        println("ziocane")
        System.exit(0)
      }
      println("phase: "+n.data.phase)
      println(n.data.move)
      println(n.data)
      if (n.data.hasWon(n.data.toMove))
        println("won")
      println(n.data.toMove+" to move:")
      i=i+1
    }
  }

  def nextMove(n : Node) : (Node,Move) = {
    val depth = if (n.data.phase==MyPhase.Phase1) 4
               else 5
    val r = alphabeta(n,depth,n.data.toMove,true)
    val firstNode = r._1.firstNode(n)
    (firstNode,firstNode.data.move)
  }

  def alphabeta(node : Node, depth : Int, toMove : Boolean, maximizingPlayer : Boolean) : Tuple2[Node,Float]=
    alphabeta(node,depth,Float.MinValue,Float.MaxValue,toMove,maximizingPlayer)
  def alphabeta(node : Node, depth : Int, currA : Float, currB: Float, toMove : Boolean, maximizingPlayer : Boolean) : Tuple2[Node,Float] = {
    var newA = currA
    var newB = currB
    var newNode = node
    if (depth == 0 || node.data.hasWon(toMove))
      return (node,node.cost)
    var toPrune : Boolean = false
    val iterator = node.childrens.iterator
    if (maximizingPlayer){
      while (iterator.hasNext && !toPrune){
        val c = iterator.next
        val r = alphabeta(c,depth-1,newA,newB,!toMove,false)
        if (r._2>newA){
          newA = r._2
          newNode = r._1
        }
        toPrune = (newB <= newA)
      }
      return (newNode,newA)
    }
    else
      for (child <- node.childrens; if(!toPrune)){
        val r = alphabeta(child,depth-1,newA,newB,!toMove,true)
        if (r._2<newB){
          newB=r._2
          newNode = r._1
        }
        toPrune =  (newB <= newA)
      }
      return (newNode,newB)
  }

  def max(a : Float, b : Float) = if (a>b) a else b
  def min(a : Float, b : Float) = if (a<b) a else b
}
