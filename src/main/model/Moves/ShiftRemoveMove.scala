package main.model.Moves

/**
 * Created by tmnd on 29/05/14.
 */
import main.model.Position

case class ShiftRemoveMove(o : Position, d : Position, toRemove : Position) extends Move{
  require (o.col == d.col || o.row == d.row)
}