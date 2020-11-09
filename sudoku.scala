import scala.io.Source
import scala.collection.mutable.ListBuffer

object Sudoku {
  //6 = 253
  var size = 6
  val board: Array[Array[Int]] = Array.ofDim[Int](size, size)
  val gaps: Array[Array[Int]] = Array.ofDim[Int](size, size)

  def main(args: Array[String]): Unit = {
    val board_source = readFile("input/board_"+size)
    val gaps_source = readFile("input/gaps_"+size)
    parseBoard(board_source)
    parseGaps(gaps_source)
    display_gaps()
    solve(0, 0)
  }

  def display_board(): Unit = {
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        if (board(i)(j) == -1){
            print("  ")
        } else {
            print(board(i)(j) + " ")
        }
      }
      println()
    }
    println()
  }

  def display_gaps(): Unit = {
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        print(gaps(i)(j) + " ")
      }
      println()
    }
    println()
  }

  def readFile(filename: String): Array[String] = {
    Source.fromFile(filename).getLines.toArray
  }

  def parseBoard(boardInput: Array[String]): Unit = {
    var row = 0
    var col = 0

    for (line <- boardInput) {
      for (c<- line) {
        board(row)(col) = c.asDigit
        col += 1
      }
      row += 1
      col = 0
    }
  }

  def parseGaps(gapsInput: Array[String]): Unit = {
    var row = 0
    var col = 0

    for (line <- gapsInput) {
      for (c<- line) {
        gaps(row)(col) = c.asDigit
        col += 1
      }
      row += 1
      col = 0
    }
  }

  def validate(row: Int, col: Int, num: Int): Boolean = {
    for (i <- 0 until size) {
      if (board(row)(i) == num || board(i)(col) == num) {
        return false
      }
    }
    true
  }

  def boardSolved(): Boolean = {
    //println("bS")
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        if (board(i)(j) == 0) {
          return false
        }
      }
    }
    true
  }

  def next(row: Int, col: Int): Boolean = {
    if (col >= (size-1)) {
      solve(row + 1, 0)
    } else {
      solve(row, col + 1)
    }
  }

  def isConsecutive(tempList: Array[Int]): Boolean = {
    for (i <- 0 until (tempList.size - 1)) {// pra não comparar o ultimo elemento
      if (tempList(i+1) != tempList(i)+1) {
          //print(tempList(i) , tempList(i+1))
        return false
      }
    }
    return true
}

  def verifySequenceCol(): Boolean = {
    //println("verify")
    var temp = new ListBuffer[String]()
    for (j <- 0 until (size)) {   // 
      for (i <- 0 until (size)) { // 
        //println(i,j)
        if (gaps(i)(j) == 1) { // aqui o I e o J podem ser igual a SIZE, o que pode dar erro
            temp += (board(i)(j)).toString
            if (j == size) {
                val tempList = (temp.toList).sorted
                //println(tempList)
                val seque = isConsecutive(tempList.map(_.toString.toInt).toArray)
                if (seque == false) {
                    //println("opa")
                    return false
                }
                temp = new ListBuffer[String]()
            }
        } else {
            val tempList = (temp.toList).sorted
            if(tempList.size > 0){
                //println(tempList)
                val seque = isConsecutive(tempList.map(_.toString.toInt).toArray)
                if (seque == false){
                    //println("opa")
                    return false
                }
            }
            temp = new ListBuffer[String]()
        }
      }
    }
    return true
  }


  def solve(row: Int, col: Int): Boolean = {
    //println(row,col)
    if (row == size-1 && col == size-1 && boardSolved()) {
      if(verifySequenceCol()){
        println("true")
        display_board()
        display_gaps()
        return true
      } else {
        //display_board()
        println("false")
      }
    } else if (board(row)(col) != 0) {
      return next(row, col)
    } else {
      for (i <- 1 to size) {
        if (validate(row, col, i)) {
          board(row)(col) = i;
          if (next(row, col)) {
            return true
          }
          board(row)(col) = 0
        }
      }
    }
    false
  }
}