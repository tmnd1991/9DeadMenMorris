package main.model.Heuristic

import main.model.MyState

/**
 * Created by tmnd on 25/05/14.
 */
class ConcreteHeuristic(val p1_nmr : Float = 10000, //factor to mul the number of morris closed before
                                  val p1_cls : Float = 10000, //factor to add if this move closes a morris
                                  val p1_owbp : Float = 2,//factor to mul the number of own pieces blocked
                                  val p1_opbp : Float = 2, //factor to mul the number of oppo pieces blocked
                                  val p1_npc : Float = 5, //factor to mul the number of pieces owned
                                  val p1_2pc : Float = 5, //factor to mul the number of pieces in the same line and the other one is empty
                                  val p1_3pc : Float = 5 //factor to mul the number of pieces that offers to close 2 morris
                                  ) extends Heuristic{
  override def calc(actual : MyState, future : MyState, player : Boolean) : Float ={
    actual.Phase match{
      case 1 => calcFirst(actual, future, player)
      case 2 => calcSecond(actual, future, player)
      case 3 => calcThird(actual, future, player)
      case _ => throw new Exception("Nonsense Phase")
    }
  }
  private def calcFirst(actual : MyState, future : MyState, player : Boolean) : Float ={
    var closedMills = 0
    if (actual!=null)
      closedMills = actual.closedMills(player)
    val closesMill = future.closedMills(player)-closedMills
    val ownBlockedPieces = future.blockedPieces(player)
    val oppoBlockedPieces = future.blockedPieces(!player)
    val ownedPieces = future.nrPieces(player)
    val twoPcsConf = 0 // TODO future.twoPcsConf(player)
    val threePcsConf = 0 //TODO future.threePcsConf(player)
    closedMills * p1_nmr + closesMill * p1_cls + ownBlockedPieces * p1_owbp + oppoBlockedPieces * p1_opbp + ownedPieces * p1_npc +
      twoPcsConf * p1_2pc + threePcsConf * p1_3pc
  }
  private def calcSecond(actual : MyState, future : MyState, player : Boolean) : Float ={
    return 1
  }

  private def calcThird(actual : MyState, future : MyState, player : Boolean) : Float ={
    return 1
  }
}
