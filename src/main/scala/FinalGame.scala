import scala.io.StdIn.readLine

package object tictactoe {
  val playerXsymbol = 'X'
  val playerOsymbol = 'O'
  val playerXName = readLine(s"You will be player X. What is your name?") //Ilze added a line
  val playerOName = readLine(s"You will be player O. What is your name?") //Ilze added a line
  val winningCombo = List((0,1,2), (3,4,5), (6,7,8),
                          (0,3,6), (1,4,7), (2,5,8),
                          (0,4,8), (2,4,6))

  //val boardNumbers = ('1' to '9').toList //Pārveidots
  var boardNumbers = List('1', '2', '3', '4', '5', '6', '7', '8','9')
  //var response = ""
  //deleted - val randomGen = new util.Random(System.currentTimeMillis)
}

package tictactoe {

  import scala.io.StdIn.readChar
 // import scala.util.control.Breaks.break

  class GameBoard(board:List[Char] = boardNumbers) {
    /**
     *
     * @param
     * @return
     */

    def movesLeft = board.filter(n => n != playerXsymbol && n != playerOsymbol)

    //deleted - def movesLeftIndex = for ((n,i) <- board.zipWithIndex if n != playerX && n != playerO) yield i
    //deleted - def computerPlays = new Board(aBoard.updated(availableMovesIdxs(randomGen.nextInt(availableMovesIdxs.length)), playerO))
    /**
     *
     * updating board by marking player X chosen numbers with symbol X
     */
    def playerXplays(move: Char) = new GameBoard(board.updated(board.indexOf(move), playerXsymbol))
    /**
     * updating board by marking player Y chosen indexes with symbol Y
     */
    def playerOplays(move: Char) = new GameBoard(board.updated(board.indexOf(move), playerOsymbol)) // replaced the computer move with 2nd player move
    /**
     *
     * @param
     * @return
     */
    def winner(winner: Char) =
      winningCombo.exists{case (i,j,k) => board(i) == winner && board(j) == winner && board(k) == winner}
    /**
     *
     * @param
     * @return
     */
    def noWinner = board.forall(n => n == playerXsymbol || n == playerOsymbol)
    /**
     *
     * @param
     * @return
     */
    def gameOver = winner(playerOsymbol) || winner(playerXsymbol) || noWinner
    /**
     *
     * @param
     * @return
     */
    def showBoard {
      board.grouped(3).foreach(row => println(row(0) + " - " + row(1) + " - " + row(2))) //Ilze changed the appearance of the board
    }
    /**
     *
     * @param
     * @return
     */
    def gameOverPrint {
      if (winner(playerXsymbol)) println(s"Congrats $playerXName, you won!") //Ilze changed the println
      else if (winner(playerOsymbol)) println(s"Congrats $playerOName you won!") //Ilze changed the println
      else if (noWinner) println("A draw - there is no winner.") //Ilze changed the println
      //deleted - else println("Something went wrong.")
    }

    //FIXME not working yet
//    def play_again(): String={
//      println("Do you want to play again? (Y/N)?")
//      val response = scala.io.StdIn.readLine()
//      if(response == "Y" || "y"){
//
//      boardNumbers = ('1' to '9').toList
//        println("Let's start again!")
//        showBoard
//      }
//      else if(response == "N" || "n"){
//        println("Until next time!")
//        break
//      }
//      else{
//        println("Please choose between Y or N")
//        play_again()
//      }
//      return response
//    }

  }

  object FinalGame extends App { //TODO jāuztaisa cits nosaukums

    println("     LET'S PLAY TIC-TAC-TOE!") //Ilze changed the println
    println
    /**
     *
     * @param
     * @return
     */
    def play(board: GameBoard, turn: Char) {

      //Reads a char from input until it is one of
      //the available moves in the current board
      /**
       *
       * @param
       * @return
       */
      def clampMove(): Char = {

        print("Choose a number from the board above -> ") ////Ilze changed the println
        val validNumbers = board.movesLeft
        val move = readChar
        if (validNumbers.contains(move)) {
          move
        } else {
          println(s"Wrong number! Choose another one from this $validNumbers")
          clampMove()
        }
      }

      println
      board.showBoard


      if (board.gameOver) {
        board.gameOverPrint
        //play_again() - not working properly yet
        return
      }

      if (turn == playerXsymbol) {
        println("   ***  PLAYER X TURN  ***   ") //Ilze changed the println
        val nextBoard = board.playerXplays(clampMove)
        play(nextBoard, playerOsymbol)
      } else {
        println("   ***  PLAYER O TURN  ***   ") //Ilze changed the println
        val nextBoard = board.playerOplays(clampMove)
        play(nextBoard, playerXsymbol)
      }
    }

    play(new GameBoard(),playerXsymbol)

  }


}


