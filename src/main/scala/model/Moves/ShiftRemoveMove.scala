package main.scala.model.Moves

/**
 * Created by tmnd on 29/05/14.
 */

import main.scala.model.Position
import it.unibo.ai.didattica.mulino.actions.{Phase2Action, Action}

case class ShiftRemoveMove(origin : Position, destination : Position, toRemove : Position) extends Move{
  require (origin.col == destination.col || origin.row == destination.row)
  override def toString : String = "ShiftRemoveMove "+origin.name+" -> "+destination.name + " // "+toRemove.name
  override def toStr : String = ShiftRemoveMove.PREFIX+origin.name+destination.name+toRemove.name
  override def toAction : Action = {
    val toRet = new Phase2Action()
    toRet.setFrom(origin.coordinates)
    toRet.setTo(destination.coordinates)
    toRet.setRemoveOpponentChecker(toRemove.coordinates)
    return toRet
  }
}

object ShiftRemoveMove{
  val PREFIX = "SR"
}