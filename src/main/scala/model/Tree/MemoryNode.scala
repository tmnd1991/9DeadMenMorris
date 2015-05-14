package main.scala.model.Tree

import main.scala.model.Heuristic.Heuristic
import main.scala.model.Moves.Move
import main.scala.model.{MyState, StateGenerator}

/**
 * Created by tmnd on 26/05/14.
 */
class MemoryNode (_data : MyState,
            private var _parent : MemoryNode = null)(implicit val costCalculator : Heuristic = Heuristic.defaultHeuristic) extends AbstractNode{

  def parent : MemoryNode = _parent

  //private var _childrens : List[MemoryNode]= null
  private var _cost : Float = -1

  def data = _data

  def cost = {
    if (_cost == -1)
      _cost = costCalculator.calc(if (parent==null) null else parent.data,data,data.toMove)
    _cost
  }

  def childrens : Iterable[MemoryNode] = StateGenerator.nextStates(data).map(s => new MemoryNode(s,this)(costCalculator))

  override def costCalculatorInstance : Heuristic = costCalculator
  def eraseParent = _parent = null
}