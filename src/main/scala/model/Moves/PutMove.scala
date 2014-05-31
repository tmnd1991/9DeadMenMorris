package main.scala.model.Moves

import main.scala.model.Position

/**
 * Created by tmnd on 29/05/14.
 */
case class PutMove(d : Position) extends Move{

  override def toString : String = "PutMove "+d.name
  override def toStr : String = PutMove.PREFIX+d.name
}
object PutMove{
  val PREFIX = "PM"
}