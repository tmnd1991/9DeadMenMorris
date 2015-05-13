package main.scala.model.Moves

import main.scala.model.Position
import it.unibo.ai.didattica.mulino.actions.{Phase1Action, Action}

/**
 * Created by tmnd on 29/05/14.
 */
case class PutRemoveMove(destination : Position, toRemove : Position) extends Move{

  override def toString : String = "PutRemoveMove "+destination.name+" // "+toRemove.name
  override def toStr : String = PutRemoveMove.PREFIX+destination.name+toRemove.name
  override def toAction : Action = {
    val toRet = new Phase1Action()
    toRet.setPutPosition(destination.coordinates)
    toRet.setRemoveOpponentChecker(toRemove.coordinates)
    return toRet
  }
}
object PutRemoveMove{
  val PREFIX = "PR"
}