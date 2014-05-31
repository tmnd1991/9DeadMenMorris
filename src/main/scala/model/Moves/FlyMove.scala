package main.scala.model.Moves

import main.scala.model.Position

/**
 * Created by tmnd on 29/05/14.
 */
case class FlyMove(o : Position, d : Position) extends Move{
  override def toString : String = "FlyMove "+o.name+" -> "+d.name
  override def toStr : String = FlyMove.PREFIX+o.name+d.name
}
object FlyMove{
  val PREFIX = "FM"
}