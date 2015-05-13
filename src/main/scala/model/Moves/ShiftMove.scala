package main.scala.model.Moves

import main.scala.model.Position
import it.unibo.ai.didattica.mulino.actions.{Phase2Action, Action}

/**
 * Created by tmnd on 29/05/14.
 */
case class ShiftMove(origin : Position, destination: Position) extends Move{
  require (origin.col == destination.col || origin.row == destination.row)
  override def toString : String = "ShiftMove "+origin.name+" -> "+destination.name
  override def toStr : String = ShiftMove.PREFIX+origin.name+destination.name
  override def toAction : Action = {
    val toRet = new Phase2Action()
    toRet.setFrom(origin.coordinates)
    toRet.setTo(destination.coordinates)
    toRet.setRemoveOpponentChecker(null)
    return toRet
  }
}
object ShiftMove{
  val PREFIX = "SM"
}