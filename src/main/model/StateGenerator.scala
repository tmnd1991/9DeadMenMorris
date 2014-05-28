package main.model

/**
 * Created by tmnd on 26/05/14.
 */

object StateGenerator {
  def nextStates(s : MyState) : List[MyState] = {
    s.Phase match {
      case 1 => phaseOneNextStates(s)
      case 2 => phaseTwoNextStates(s)
      case 3 => phaseThreeNextStates(s)
      case _ => throw new IllegalArgumentException("Nonsense Phase")
    }
  }

  private def phaseOneNextStates(s : MyState) : List[MyState] = {
    var toRet = List[MyState]()
    for (p <- s.emptyPositions){
      if (s.moveCreatesMill(p,s.toMove)){
        for(pp <- s.removablePieces(!s.toMove))
          toRet ::= s.stateByPutting(p,Some(pp))
      }
      else
        toRet ::= s.stateByPutting(p)
    }
    toRet
  }

  private def phaseTwoNextStates(s : MyState) : List[MyState] = {
    var toRet = List[MyState]()
    val pcs = s.pieces(s.toMove)
    for (pc <- pcs){
      val possibleDest = pc.neighbourhood(0).map(p => s.positions(p)).filter(p => p.content==None) :::
        pc.neighbourhood(1).map(p => s.positions(p)).filter(p => p.content==None)
      for(p <- possibleDest){
        if(s.moveCreatesMill(pc,p,s.toMove))
          for(pp <- s.removablePieces(!s.toMove))
            toRet ::= s.stateByMoving(pc,p,Some(pp))
        else
          toRet ::= s.stateByMoving(pc,p)
      }
    }
    toRet
  }

  private def phaseThreeNextStates(s : MyState) : List[MyState] = {
    //sono io in fase 3 o l'altro ? :D
    val myPcs = s.pieces(s.toMove)
    if (myPcs.length>3)
      phaseTwoNextStates(s)
    else{
      var toRet = List[MyState]()
      for (pp <- myPcs;
           p <- s.emptyPositions) {
        if (s.moveCreatesMill(p, s.toMove)) {
          for (pp <- s.removablePieces(!s.toMove))
            toRet ::= s.stateByMoving(pp, p, Some(pp))
        }
        else
            toRet ::= s.stateByMoving(pp, p)
      }
      toRet
    }
  }

}
