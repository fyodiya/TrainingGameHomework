
package object tictactoe {
  val playerX = 'X' //Ilze changed the name
  val playerO = 'O' //Ilze changed the name
  val BaseBoard = ('1' to '9').toList
  val WinnerLines = List((0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6))
  //val randomGen = new util.Random(System.currentTimeMillis) - deleted
}

package tictactoe {

  import scala.io.StdIn.readChar

  class Board(aBoard : List[Char] = BaseBoard) {

    def availableMoves = aBoard.filter(c => c != playerX && c != playerO)

    def availableMovesIdxs = for ((c,i) <- aBoard.zipWithIndex if c != playerX && c != playerO) yield i

    //deleted - def computerPlays = new Board(aBoard.updated(availableMovesIdxs(randomGen.nextInt(availableMovesIdxs.length)), playerO))
    def playerOplays(move : Char) = new Board(aBoard.updated(aBoard.indexOf(move), playerO)) //Ilze changed the name
    // and replaced the computer move with 2nd player move

    def playerXplays(move : Char) = new Board(aBoard.updated(aBoard.indexOf(move), playerX)) //Ilze changed the name

    def isDraw = aBoard.forall(c => c == playerX || c == playerO)

    def isWinner(winner : Char) =
      WinnerLines.exists{case (i,j,k) => aBoard(i) == winner && aBoard(j) == winner && aBoard(k) == winner}

    def isOver = isWinner(playerO) || isWinner(playerX) || isDraw

    def print {
      aBoard.grouped(3).foreach(row => println(row(0) + " " + row(1) + " " + row(2)))
    }

    def printOverMessage {
      if (isWinner(playerX)) println("PLAYER X WINS.") //Ilze changed the println
      else if (isWinner(playerO)) println("PLAYER 0 WINS.") //Ilze changed the println
      else if (isDraw) println("A draw - there is no winner.") //Ilze changed the println
      //else println("Not over yet, or something went wrong.") - deleted
    }

  }


  object FinalGame extends App {

    println("LET'S PLAY TIC-TAC-TOE! HERE IS THE STARTING BOARD WITH NUMBERS") //Ilze changed the println

    def play(board : Board, turn : Char) {

      //Reads a char from input until it is one of
      //the available moves in the current board
      def clampMove() : Char = {

        print("Choose your move: ") ////Ilze changed the println
        val validMoves = board.availableMoves
        val move = readChar
        if (validMoves.contains(move)) {
          move
        } else {
          println("Invalid move. Choose another one from " + validMoves)
          clampMove()
        }
      }


      board.print

      if (board.isOver) {
        board.printOverMessage
        return
      }

      if (turn == playerX) {
        println("** PLAYER X TURN **") //Ilze changed the println
        val nextBoard = board.playerXplays(clampMove)
        play(nextBoard, playerO)
      } else {
        println("** PLAYER O TURN **") //Ilze changed the println
        val nextBoard = board.playerOplays(clampMove)
        play(nextBoard, playerX)
      }
    }

    play(new Board(),playerX)

  }


}


