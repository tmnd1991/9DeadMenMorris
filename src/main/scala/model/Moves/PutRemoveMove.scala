package main.scala.model.Moves

import main.scala.model.Position

/**
 * Created by tmnd on 29/05/14.
 */
case class PutRemoveMove(d : Position, toRemove : Position) extends Move{

  override def toString : String = "PutRemoveMove "+d.name+" // "+toRemove.name
  override def toStr : String = PutRemoveMove.PREFIX+d.name+toRemove.name
}
object PutRemoveMove{
  val PREFIX = "PR"
}