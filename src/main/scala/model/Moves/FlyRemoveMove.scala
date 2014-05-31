package main.scala.model.Moves

import main.scala.model.Position

/**
 * Created by tmnd on 29/05/14.
 */
case class FlyRemoveMove(o : Position, d : Position, toRemove : Position) extends Move{
  override def toString : String = "FlyRemoveMove "+o.name+" -> "+d.name +" // "+toRemove.name
  override def toStr : String = FlyRemoveMove.PREFIX+o.name+d.name+toRemove.name
}

object FlyRemoveMove{
  val PREFIX = "FR"
}

