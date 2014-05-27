package main.model.Heuristic

import main.model.MyState

abstract class Heuristic {
  def calc(actual : MyState, future : MyState, player : Boolean) : Float
}