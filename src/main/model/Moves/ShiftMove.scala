package main.model.Moves

import main.model.Position
/**
 * Created by tmnd on 29/05/14.
 */
case class ShiftMove(o : Position, d: Position) extends Move{
  require (o.col == d.col || o.row == d.row)
  override def toString : String = "ShiftMove "+o.name+" -> "+d.name
}
