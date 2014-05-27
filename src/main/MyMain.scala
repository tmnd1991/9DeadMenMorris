package main


import main.model.{StateGenerator, MyState}
import main.model.Heuristic.{ConcreteHeuristic, Heuristic}
import main.model.Tree.Node



/**
 * Created by tmnd on 24/05/14.
 */
object MyMain {
  def main(args : Array[String]) = {
    var n = new Node(new MyState(true,""),new ConcreteHeuristic())
    var i = 0
    var toMove = true
    println(n.data)
    while(i<1000){
      println(i)
      n = nextMove(n,toMove)._1
      println(toMove+" to move:")
      println(n.data.stringMove)
      println(n.data)
      if (n.data.hasWon(toMove))
        println("won")
      n.eraseParent
      toMove = !toMove
      i=i+1
    }

  }

  def nextMove(n : Node, toMove: Boolean) : Tuple2[Node,String] = {
    val r = alphabeta(n,5,toMove,true)
    (r._1.firstNode,r._1.firstMove)
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
    if (maximizingPlayer){
      for {c <- node.childrens
           if(!toPrune)}{
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
