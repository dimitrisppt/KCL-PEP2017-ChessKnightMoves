// Part 1 about finding and counting Knight's tours
//==================================================

object CW7a {

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


//some test cases

// assert(legal_moves(8, Nil, (2,2)) ==
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
// assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
// assert(legal_moves(8, List((4,1), (1,0)), (2,2)) ==
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
// assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(1c) Complete the two recursive functions below.
//     They exhaustively search for knight's tours starting from the
//     given path. The first function counts all possible tours,
//     and the second collects all tours in a list of paths.

 def count_tours(dim: Int, path: Path) : Int = {

     val moves = legal_moves(dim, path, path.head)
     if (legal_moves(dim, List(path.head), path.head).contains(path.last) && dim*dim == path.size) {
        0
    } else if ((!(legal_moves(dim, List(path.head), path.head).contains(path.last))) && dim*dim == path.size) {
        1
    } else {
        (for (eachMove <- moves)
            yield count_tours(dim, eachMove::path))
            .sum
    }
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
    val moves = legal_moves(dim, path, path.head)
        if (legal_moves(dim, List(path.head), path.head).contains(path.last) && dim*dim == path.size) {
            Nil
        } else if ((!(legal_moves(dim, List(path.head), path.head).contains(path.last))) && dim*dim == path.size) {
           List(path)
        } else {
            (for (eachMove <- moves)
                yield enum_tours(dim, eachMove::path))
                .flatten
        }
}

}
