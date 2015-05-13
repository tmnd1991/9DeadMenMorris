package main.scala.model.Moves

import main.scala.model.Position
import it.unibo.ai.didattica.mulino.actions.{PhaseFinalAction, Action}

/**
 * Created by tmnd on 29/05/14.
 */
case class FlyRemoveMove(origin : Position, destination : Position, toRemove : Position) extends Move{
  override def toString : String = "FlyRemoveMove "+destination.name+" -> "+destination.name +" // "+toRemove.name
  override def toStr : String = FlyRemoveMove.PREFIX+origin.name+destination.name+toRemove.name
  override def toAction : Action = {
    val toRet = new PhaseFinalAction()
    toRet.setFrom(origin.coordinates)
    toRet.setTo(destination.coordinates)
    toRet.setRemoveOpponentChecker(toRemove.coordinates)
    return toRet
  }
}

object FlyRemoveMove{
  val PREFIX = "FR"
}

