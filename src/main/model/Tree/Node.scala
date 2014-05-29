package main.model.Tree

import groovy.lang.Tuple
import main.model.{MyState, StateGenerator}
import main.model.Heuristic.Heuristic
import main.model.Moves.Move

/**
 * Created by tmnd on 26/05/14.
 */
class Node (_data : => MyState,
            costCalculator : Heuristic,
            val parent : Node = null,
            val player : Boolean = false){

  val isEmpty = parent==null

  private var _childrens : List[Node]= null
  private var _cost : Float = -1

  def data = _data

  def cost = {
    if (_cost == -1)
      _cost = costCalculator.calc(if (parent==null) null else parent.data,data,data.toMove)
    _cost
  }

  def childrens : Iterable[Node] = {
    if (_childrens==null)
      _childrens = StateGenerator.nextStates(data).map(s => new Node(s,costCalculator,this))
    _childrens
  }

  def firstNode(p : Node) : Node = {
    if (parent==p)
      return this
    else
      parent.firstNode(p)
  }

  def firstMove(p : Node) : Move = {
    firstNode(p).data.move
  }
}