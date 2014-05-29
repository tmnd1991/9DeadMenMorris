package main.model.Moves

import main.model.Position
/**
 * Created by tmnd on 29/05/14.
 */
case class FlyRemoveMove(o : Position, d : Position, toRemove : Position) extends Move{
  override def toString : String = "FlyRemoveMove "+o.name+" -> "+d.name +" // "+toRemove.name
}

