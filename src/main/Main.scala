package main

import main.model.MyState

/**
 * Created by tmnd on 24/05/14.
 */
object MyMain {
  def main(args : Array[String]) = {
    var s = new MyState()
    s = s.stateByPutting(s.positions("A7"),true).
          stateByPutting(s.positions("A4"),true).
          stateByPutting(s.positions("A1"),true).
          stateByPutting(s.positions("D1"),true).
          stateByPutting(s.positions("G1"),true).
          stateByPutting(s.positions("D3"),true).
          stateByPutting(s.positions("D2"),true)
    println(s)
    println(s.closedMills(true))


  }
}
