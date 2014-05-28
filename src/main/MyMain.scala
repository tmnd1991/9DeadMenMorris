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
    println(n.data)
    while(i<1000){
      println(i)
      n = nextMove(n)._1
      println(n.data.toMove+" to move:")
      println(n.data.stringMove)
      println(n.data)
      if (n.data.hasWon(n.data.toMove))
        println("won")
      n.eraseParent
      i=i+1
      //Console.readLine()
    }

  }

  def nextMove(n : Node) : Tuple2[Node,String] = {
    val deep = if (n.data.Phase==1) 5
               else 9
    val r = alphabeta(n,deep,n.data.toMove,true)
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
