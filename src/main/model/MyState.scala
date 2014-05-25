package main.model

class MyState (private var _phase : Int = 1,
               val positions : Map[String,Position] = MyState.emptyPositions,
               val removed : Map[Boolean,Int] = Map(true->0,false->0)){

  def Phase : Int = _phase

  def Phase_=(p : Int) : Unit = {
      require(p>0 && p<4)
      _phase = p
  }

  def stateByPutting(p : Position, c : Boolean, toRemove : Option[Position] = None) : MyState = {
      require(positions(p.name).content == None)
      require(Phase==1)
      require(isLegalToRemove(toRemove,c))
      val newMap = collection.mutable.Map(positions.toSeq: _*)
      val newRemoved = collection.mutable.Map(removed.toSeq: _*)
      newMap.remove(p.name)
      newMap(p.name) = new Position(p.name,Some(c))
      if (toRemove!=None){
        newMap.remove(toRemove.get.name)
        newMap(toRemove.get.name)=new Position(toRemove.get.name)
        newRemoved(toRemove.get.content.get) = newRemoved(toRemove.get.content.get)+1
      }
      val newPhase = if (onBoard(true)+eaten(true)+onBoard(false)+eaten(false) == 18) 2
                     else 1
      new MyState(newPhase,Map(newMap.toList: _*),Map(newRemoved.toList: _*))
  }

  def stateByMoving(o : Position, d : Position, c : Boolean, toRemove : Option[Position] = None) : MyState = {
    require(positions(d.name).content == None) // la casella deve essere vuota
    require(positions(o.name).content != None) // la casella di partenza deve essere piena
    require(positions(o.name).content.get == c)// deve esserci una pedina del giocatore che muove ;)
    if (Phase == 2 || onBoard(c)>3) //anche se siamo in phase3 ma io ho più pedine non posso muovere ovunque ;)
      require(positions(o.name).isNeighbourOf(positions(d.name)))
    //require mossa possibile, diversa tra phase2 and phase3
    require(Phase==2 || Phase==3)
    require(isLegalToRemove(toRemove,c))
    val newMap = collection.mutable.Map(positions.toSeq: _*)
    val newRemoved = collection.mutable.Map(removed.toSeq: _*)
    newMap.remove(o.name)
    newMap(o.name) = new Position(o.name)
    newMap.remove(d.name)
    newMap(d.name) = new Position(d.name,Some(c))
    if (toRemove!=None){
      newMap.remove(toRemove.get.name)
      newMap(toRemove.get.name)=new Position(toRemove.get.name)
      newRemoved(toRemove.get.content.get) = newRemoved(toRemove.get.content.get)+1
    }
    val newPhase = if (onBoard(!c)==3) 3
                   else 2
    new MyState(newPhase,Map(newMap.toList: _*), Map(newRemoved.toList: _*))
  }

  def onBoard(c : Boolean) : Int = positions.values.count(p => p.content==Some(c))

  def eaten(c : Boolean) : Int = removed(c)

  def isLegalToRemove(p : Option[Position], c : Boolean) : Boolean = {
      p match {
        case None => true
        case _ => !isPartOfMill(p.get,!c)
      }
  }

  def removable(c : Boolean) : List[Position] = positions.values.filter(p => isLegalToRemove(Some(p),!c)).toList

  def hasWon(c : Boolean) : Boolean = onBoard(!c)<3 || cantMove(!c)

  def hasLost(c : Boolean) : Boolean = hasWon(!c)

  def cantMove(c : Boolean) : Boolean = {
    var free = 0
    for (p <- positions.values.filter(p => p.content == Some(c))){

      free+=(p.neighbourhood(0).map(s => getPosition(s)) ++
              p.neighbourhood(1).map(s => getPosition(s))).
        count(pp => pp.content==None) //conto le posizione vicine libere
      if (free>0) //se sono più di zero posso muovermi
        return false
    }
    free==0 //se sono zero allora non posso muovermi :( LOST
  }


  private var _pieces = scala.collection.mutable.Map[Boolean,Option[List[Position]]](true->None,false->None)
  def pieces(c : Boolean) : List[Position] = {
    if (_pieces(c) == None)
      _pieces(c) = Some(positions.values.filter(p => p.content == Some(c)).toList)
    _pieces(c).get
  }

  def nrPieces(c : Boolean) : Int = {
    if (_pieces(c)==None)
      pieces(c)
    _pieces(c).get.size
  }

  def closedMills(c : Boolean) : Int = {
    var toCheck = pieces(c)
    var toRet = 0
    while(toCheck.size>0){
      val p = toCheck.head
      require(p.content!=None)
      require(p.content.get==c)
      if (isPartOfMillR(p,c,List(),0)){
        toCheck = toCheck.filterNot(pp => pp isInCol p.col)
        toRet += 1
      }
      if (isPartOfMillR(p,c,List(),1)){
        toCheck = toCheck.filterNot(pp => pp isInRow p.row)
        toRet += 1
      }
    }
    toRet
  }

  def isPartOfMill(p : Position, c : Boolean) : Boolean = {
      require(p.content!=None)
      require(p.content.get==c)
      val nsRes = isPartOfMillR(p,c,List(),0)
      val weRes = isPartOfMillR(p,c,List(),1)
      nsRes || weRes
  }

   //AM I THAT GOOD? :D
  def isPartOfMillR(p : Position, c : Boolean, excluded : List[Position], d : Int) : Boolean = {
      if (excluded.size==2 && p.content == Some(c))
        true
      else {
        if (p.content == Some(c)) {
          for {pos <- p.neighbourhood(d).map(s => getPosition(s))
               if (!excluded.contains(pos))}
            return isPartOfMillR(pos, c, (excluded.::(p)), d)
        }
        return false
      }
  }

  def getPosition(s : String) : Position = {
      if (positions.contains(s))
          positions(s)
      else
          throw new IllegalArgumentException()
  }

  override def toString : String = {
    var toRet = ""
    toRet+=getPosition("A7")+"--"+getPosition("D7")+"--"+getPosition("G7")+"\n"
    toRet+="-"+getPosition("B6")+"-"+getPosition("D6")+"-"+getPosition("F6")+"-\n"
    toRet+="--"+getPosition("C5")+getPosition("D5")+getPosition("E5")+"--\n"
    toRet+=""+getPosition("A4")+getPosition("B4")+getPosition("C4")+"-"+
              getPosition("E4")+getPosition("F4")+getPosition("G4")+"\n"
    toRet+="--"+getPosition("C3")+getPosition("D3")+getPosition("E3")+"--\n"
    toRet+="-"+getPosition("B2")+"-"+getPosition("D2")+"-"+getPosition("F2")+"-\n"
    toRet+=getPosition("A1")+"--"+getPosition("D1")+"--"+getPosition("G1")+"\n"
    toRet
  }
}

object MyState{
    def emptyPositions : Map[String,Position] = {
        Map(
        "A1" -> new Position("A1"),
        "D1" -> new Position("D1"),
        "G1" -> new Position("G1"),
        "A7" -> new Position("A7"),
        "D7" -> new Position("D7"),
        "G7" -> new Position("G7"),
        
        "B2" -> new Position("B2"),
        "D2" -> new Position("D2"),
        "F2" -> new Position("F2"),
        "B6" -> new Position("B6"),
        "D6" -> new Position("D6"),
        "F6" -> new Position("F6"),
        
        "C3" -> new Position("C3"),
        "D3" -> new Position("D3"),
        "E3" -> new Position("E3"),
        "C5" -> new Position("C5"),
        "D5" -> new Position("D5"),
        "E5" -> new Position("E5"),
        
        "A4" -> new Position("A4"),
        "B4" -> new Position("B4"),
        "C4" -> new Position("C4"),
        "E4" -> new Position("E4"),
        "F4" -> new Position("F4"),
        "G4" -> new Position("G4")
        )
    }
}