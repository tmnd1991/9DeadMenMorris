package main.scala.model.Moves

/**
 * Created by tmnd on 29/05/14.
 */

import main.scala.model.Position

case class ShiftRemoveMove(o : Position, d : Position, toRemove : Position) extends Move{
  require (o.col == d.col || o.row == d.row)
  override def toString : String = "ShiftRemoveMove "+o.name+" -> "+d.name + " // "+toRemove.name
  override def toStr : String = ShiftRemoveMove.PREFIX+d.name+o.name+toRemove.name
}

object ShiftRemoveMove{
  val PREFIX = "SR"
}