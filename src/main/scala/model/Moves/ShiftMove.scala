package main.scala.model.Moves

import main.scala.model.Position

/**
 * Created by tmnd on 29/05/14.
 */
case class ShiftMove(o : Position, d: Position) extends Move{
  require (o.col == d.col || o.row == d.row)
  override def toString : String = "ShiftMove "+o.name+" -> "+d.name
  override def toStr : String = ShiftMove.PREFIX+d.name+o.name
}
object ShiftMove{
  val PREFIX = "SM"
}