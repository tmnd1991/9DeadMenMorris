package main.model.Moves

import main.model.Position
/**
 * Created by tmnd on 29/05/14.
 */
case class PutRemoveMove(d : Position, toRemove : Position) extends Move{
  override def toString : String = "PutRemoveMove "+d.name+" // "+toRemove.name
}
