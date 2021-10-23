import scala.io.StdIn.readLine


package object tictactoe {
  val playerXsymbol = 'X'
  val playerOsymbol = 'O'
  val playerXName = readLine(s"You will be player X. What is your name?")
  val playerOName = readLine(s"You will be player O. What is your name?")
  val winningCombo = Array((0,1,2), (3,4,5), (6,7,8),
                          (0,3,6), (1,4,7), (2,5,8),
                          (0,4,8), (2,4,6))

  var boardNumbers = Array('1', '2', '3', '4', '5', '6', '7', '8','9')

}

package tictactoe {

  import scala.io.StdIn.readChar


  class GameBoard(classBoard:Array[Char] = boardNumbers) {
    /**
     *
     * Printing array in a form of game board
     *
     */
    def showBoard {
      println(classBoard(0) + " | " + classBoard(1) + " | " + classBoard(2))
      println(classBoard(3) + " | " + classBoard(4) + " | " + classBoard(5))
      println(classBoard(6) + " | " + classBoard(7) + " | " + classBoard(8))
    }

    /**
     *
     * updating board by replacing player X chosen number index with symbol X
     */
    def playerXplays(move: Char) = new GameBoard(classBoard.updated(classBoard.indexOf(move), playerXsymbol))
    /**
     * updating board by replacing player Y chosen number index with symbol X
     */
    def playerOplays(move: Char) = new GameBoard(classBoard.updated(classBoard.indexOf(move), playerOsymbol))
    /**
     *
     * Checking if any of the winning combo is filled
     */
    def winner(winner: Char) =
      winningCombo.exists{case (i,j,k) => classBoard(i) == winner && classBoard(j) == winner && classBoard(k) == winner}
    /**
     *
     * @param
     * @return
     */
    def noWinner = classBoard.forall(n => n == playerXsymbol || n == playerOsymbol)

    def movesLeft = classBoard.filter(n => n != playerXsymbol && n != playerOsymbol)


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
    def gameOverPrint {
      if (winner(playerXsymbol)) println(s"Congrats $playerXName, you won!")
      else if (winner(playerOsymbol)) println(s"Congrats $playerOName you won!")
      else if (noWinner) println("A draw - there is no winner.")
    }



    //
//    def play_again(): Unit={
//      response = readLine("Do you want to play again? (yes or no)?")
//      if(response == "yes"){
//        resetBoard
//        println("Let's start again!")
//        showBoard
//      }
//      else if(response == "no"){
//        println("Have a nice day!")
//        break
//      }
//      else{
//        println("Please choose between yes or no")
//        play_again()
//      }
//      return response
//    }

  }
  import scala.util.control.Breaks.break
  object FinalGame extends App { //TODO jāuztaisa cits nosaukums
    println("     LET'S PLAY TIC-TAC-TOE!")
    println
    /**
     *
     * @param
     * @return
     */

    def resetBoard(empty: GameBoard)={
      boardNumbers = Array()
      boardNumbers = Array('1', '2', '3', '4', '5', '6', '7', '8','9')
    }
    var is_game_needed = true
    def play(board: GameBoard, turn: Char) {
      //Reads a char from input until it is one of
      //the available moves in the current board
      /**
       *
       * @param
       * @return
       */
      def clampMove(): Char = {

        print("Choose a number from the board above -> ")
        val validNumbers = board.movesLeft
        val move = readChar
        if (validNumbers.contains(move)) {
          move
        } else {
          println(s"Wrong number! Choose something from available options")
          clampMove()
        }
      }
      while (is_game_needed) { //PLAY AGAIN LOOP
        println
        board.showBoard

        if (board.gameOver) {
          board.gameOverPrint
          val response = readLine("DO YOY WANT TO PLAY AGAIN? TYPE Y OR N -> ")
           if (response.startsWith("y") || response.startsWith("Y")) {
              println
              println("Great! Let's start again!")
              resetBoard(new GameBoard())
              play(new GameBoard(),playerXsymbol)
           }
           else
             println
             println("Thanks for playing. See you next time!")
             break //END OF THE PLAY AGAIN LOOP
        }

        if (turn == playerXsymbol) {
          println("   ***  PLAYER X TURN  ***   ")
          val nextBoard = board.playerXplays(clampMove)
          play(nextBoard, playerOsymbol)
        } else {
          println("   ***  PLAYER O TURN  ***   ")
          val nextBoard = board.playerOplays(clampMove)
          play(nextBoard, playerXsymbol)
        }
      }
    }




//    def mainGameLoop(): Unit = {
//      var is_game_needed = true
//      while (is_game_needed) {
//        play(new GameBoard(),playerXsymbol)
//        //afterGame()
//        val response = readLine("New game Y/N ?")
//        if (response.toLowerCase.startsWith("y")) {
////          val newPlayerResponse = readLine("New Players ? Y/N")
////          if (newPlayerResponse.toLowerCase.startsWith("y")) {
////            initPlayerSettings()
//       //   }
//          play(new GameBoard(),playerXsymbol) }//we need to reset game state no matter if we have new players
//   //       displayGameStats()
//    //    }
//        else is_game_needed = false  //in this case we end the game on anything other text starting with y or Y
//      }
//
//    }

    play(new GameBoard(),playerXsymbol)







  }


}


