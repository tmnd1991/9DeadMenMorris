package main.scala.model.Moves

import main.scala.model.Position
import it.unibo.ai.didattica.mulino.actions._
/**
 * Created by tmnd on 29/05/14.
 */
case class FlyMove(origin : Position, destination : Position) extends Move{
  override def toString : String = "FlyMove "+origin.name+" -> "+destination.name
  override def toStr : String = FlyMove.PREFIX+origin.name+destination.name
  override def toAction : Action = {
    val toRet = new PhaseFinalAction()
    toRet.setFrom(origin.coordinates)
    toRet.setTo(destination.coordinates)
    toRet.setRemoveOpponentChecker(null)
    return toRet
  }
}
object FlyMove{
  val PREFIX = "FM"
}