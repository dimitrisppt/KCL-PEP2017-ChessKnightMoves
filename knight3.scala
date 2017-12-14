// Part 3 about finding a single tour using the Warnsdorf Rule
//=============================================================

// copy any function you need from files knight1.scala and
// knight2.scala

object CW7c {

type Pos = (Int, Int)    // a position on a chessboard
type Path = List[Pos]    // a path...a list of positions

//(1a) Complete the function that tests whether the position
//     is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {
    val legalPath = path.find(yCord => yCord == x).isDefined
    val insideBoard = (x._1 < dim) && (x._2 < dim) && (x._1 >= 0) && (x._2 >= 0)
    (!legalPath && insideBoard)
}

//(1b) Complete the function that calculates for a position
//     all legal onward moves that are not already in the path.
//     The moves should be ordered in a "clockwise" manner.

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

    val a = (x._1+1, x._2+2); val b = (x._1+2, x._2+1); val c = (x._1+2, x._2-1); val d = (x._1+1, x._2-2); val e = (x._1-1, x._2-2); val f = (x._1-2, x._2-1); val g = (x._1-2, x._2+1); val h = (x._1-1, x._2+2)
    val lst = List(a,b,c,d,e,f,g,h).filter(currentMove => is_legal(dim, path)(currentMove))
    lst
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
    if (xs.size == 0) {
        None
    } else {
        val fx = f(xs.head)
        if (fx != None) {
            fx
        } else {
            first(xs.drop(1), f)
        }
    }
}

//(3a) Complete the function that calculates a list of onward
//     moves like in (1b) but orders them according to Warnsdorf’s
//     rule. That means moves with the fewest legal onward moves
//     should come first.

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val ordered_legal_moves = legal_moves(dim, path, x)
    ordered_legal_moves.sortWith(legal_moves(dim, path, _).size < legal_moves(dim, path, _).size)
}


//(3b) Complete the function that searches for a single *closed*
//     tour using the ordered moves function.

// def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
//
// }


//(3c) Same as (3b) but searches for *non-closed* tours. However,
//     you have to be careful to write a tail-recursive version as this
//     function will be called with dimensions of up to 40 * 40.

//def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = ...


}
