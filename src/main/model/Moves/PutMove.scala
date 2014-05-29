package main.model.Moves

import main.model.Position
/**
 * Created by tmnd on 29/05/14.
 */
case class PutMove(d : Position) extends Move{
  override def toString : String = "PutMove "+d.name
}
