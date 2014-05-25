package main.model

abstract class FirstPhaseHeuristic(val f_cls : Float, //factor to add if this move closes a morris
                val f_nmr : Float, //factor to mul the number of morris closed before
                val f_nbp : Float, //factor to mul the number of oppo pieces blocked
                val f_npc : Float, //factor to mul the number of pieces owned
                val f_2pc : Float, //factor to mul the number of pieces in the same line and the other one is empty
                val f_3pc : Float //factor to mul the number of pieces that offers to close 2 morris
                ) {
    def calc(s : MyState) : Double
}