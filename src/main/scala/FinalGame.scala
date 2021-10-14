import scala.io.StdIn.readLine

package object tictactoe {
  val playerX = 'X'
  val playerO = 'O'
  val playerXName = readLine(s"You will be player X. What is your name?") //Ilze added a line
  val playerOName = readLine(s"You will be player O. What is your name?") //Ilze added a line
  val winningOptions = List((0,3,6), (1,4,7), (2,5,8), (0,1,2), (3,4,5), (6,7,8), (0,4,8), (2,4,6))
  val boardNumbers = ('1' to '9').toList

  //deleted - val randomGen = new util.Random(System.currentTimeMillis)
}

package tictactoe {

  import scala.io.StdIn.readChar

  class GameBoard(board:List[Char] = boardNumbers) {


    def movesLeft = board.filter(n => n != playerX && n != playerO)

    //deleted - def movesLeftIndex = for ((n,i) <- board.zipWithIndex if n != playerX && n != playerO) yield i
    //deleted - def computerPlays = new Board(aBoard.updated(availableMovesIdxs(randomGen.nextInt(availableMovesIdxs.length)), playerO))

    def playerXplays(move: Char) = new GameBoard(board.updated(board.indexOf(move), playerX))

    def playerOplays(move: Char) = new GameBoard(board.updated(board.indexOf(move), playerO)) // replaced the computer move with 2nd player move



    def winner(winner: Char) =
      winningOptions.exists{case (i,j,k) => board(i) == winner && board(j) == winner && board(k) == winner}

    def draw = board.forall(c => c == playerX || c == playerO)

    def gameOver = winner(playerO) || winner(playerX) || draw

    def boardPrint {
      board.grouped(3).foreach(row => println(row(0) + " - " + row(1) + " - " + row(2))) //Ilze changed the appearance of the board
    }

    def gameOverPrint {
      if (winner(playerX)) println(s"Congrats $playerXName, you won!") //Ilze changed the println
      else if (winner(playerO)) println(s"Congrats $playerOName you won!") //Ilze changed the println
      else if (draw) println("A draw - there is no winner.") //Ilze changed the println
      //deleted - else println("Something went wrong.")
    }

  }

  object FinalGame extends App { //need to rename

    println("     LET'S PLAY TIC-TAC-TOE!") //Ilze changed the println
    println

    def play(board: GameBoard, turn: Char) {

      //Reads a char from input until it is one of
      //the available moves in the current board
      def clampMove(): Char = {

        print("Choose a number from the board above -> ") ////Ilze changed the println
        val validNumbers = board.movesLeft
        val move = readChar
        if (validNumbers.contains(move)) {
          move
        } else {
          println("Wrong number! Choose another one from this" + validNumbers)
          clampMove()
        }
      }

      println
      board.boardPrint


      if (board.gameOver) {
        board.gameOverPrint
        return
      }

      if (turn == playerX) {
        println("   ***  PLAYER X TURN  ***   ") //Ilze changed the println
        val nextBoard = board.playerXplays(clampMove)
        play(nextBoard, playerO)
      } else {
        println("   ***  PLAYER O TURN  ***   ") //Ilze changed the println
        val nextBoard = board.playerOplays(clampMove)
        play(nextBoard, playerX)
      }
    }

    play(new GameBoard(),playerX)

  }


}


