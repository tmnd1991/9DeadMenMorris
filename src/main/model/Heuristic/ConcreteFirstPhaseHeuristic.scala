package main.model.Heuristic

import main.model.MyState

/**
 * Created by tmnd on 25/05/14.
 */
class ConcreteFirstPhaseHeuristic(val f_nmr : Float, //factor to mul the number of morris closed before
                                  val f_cls : Float, //factor to add if this move closes a morris
                                  val f_owbp : Float,//factor to mul the number of own pieces blocked
                                  val f_opbp : Float, //factor to mul the number of oppo pieces blocked
                                  val f_npc : Float, //factor to mul the number of pieces owned
                                  val f_2pc : Float, //factor to mul the number of pieces in the same line and the other one is empty
                                  val f_3pc : Float //factor to mul the number of pieces that offers to close 2 morris
                                  ) extends Heuristic{
  override def calc(actual : MyState, future : MyState, player : Boolean) : Double ={
    val closedMills = actual.closedMills(player)
    val closesMill = future.closedMills(player)-closedMills
    val ownBlockedPieces = future.blockedPieces(player)
    val oppoBlockedPieces = future.blockedPieces(!player)
    val ownedPieces = future.nrPieces(player)
    val twoPcsConf = future.twoPcsConf(player)
    val threePcsConf = future.threePcsConf(player)
    closedMills * f_nmr + closesMill * f_cls + ownBlockedPieces * f_owbp + oppoBlockedPieces * f_opbp + ownedPieces * f_npc +
      twoPcsConf * f_2pc + threePcsConf * f_3pc
  }

}
