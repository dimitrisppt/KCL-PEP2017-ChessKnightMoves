// Part 2 about finding a single tour for a board
//================================================

// copy any function you need from file knight1.scala

object CW7b {

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

//(2a) Implement a first-function that finds the first
//     element, say x, in the list xs where f is not None.
//     In that case Return f(x), otherwise None. If possible,
//     calculate f(x) only once.

def f = (x:(Int, Int)) => if (x._1 > 3) Some(List(x)) else None

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

//(2b) Implement a function that uses the first-function for
//     trying out onward moves, and searches recursively for a
//     knight tour on a dim * dim-board.

def first_tour(dim: Int, path: Path) : Option[Path] = {

}

}
