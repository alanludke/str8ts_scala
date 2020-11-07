import scala.io.Source
import scala.collection.mutable.ListBuffer

object Sudoku {
  var size = 6
  val board: Array[Array[Int]] = Array.ofDim[Int](size, size)
  val gaps: Array[Array[Int]] = Array.ofDim[Int](size, size)

  def main(args: Array[String]): Unit = {
    val board_source = readFile("input/board_"+size)
    val gaps_source = readFile("input/gaps_"+size)
    parseBoard(board_source)
    solve(0, 0)
    display_board()
    parseGaps(gaps_source)
    display_gaps()
    val ord = verifySequence()
    println(ord)
  }

  def display_board(): Unit = {
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        print(board(i)(j) + " ")
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
    if (col >= size-1) {
      solve(row + 1, 0)
    } else {
      solve(row, col + 1)
    }
  }

  def isConsecutive(tempList: Array[Int]): Boolean = {
    for (i <- 0 until (size - 1)) {// pra não comparar o ultimo elemento
      if (tempList(i) != tempList(i+1)) {
        return false  // 
      }
    }
    return true
}

/*
def isConsecutive(seq: Array[Int]): (Boolean, Int) = {
  val count = seq.sliding(2).count(a => a(0)+1 == a(1)) 
  (count > 0, count)
}
*/
  def verifySequence(): Unit = {
    for (i <- 0 until size) {
      var temp = new ListBuffer[String]()
      for (j <- 0 until size) {
        if (gaps(j)(i) == 1) {
            temp += (board(j)(i)).toString
            if (j == size-1) {
                val tempList = (temp.toList).sorted
                println(tempList)
                val seque = isConsecutive(tempList.map(_.toString.toInt).toArray)
                if (seque == false){
                    println("opa")
                    //return false
                }
                temp = new ListBuffer[String]()
            }
        } else {
            val tempList = (temp.toList).sorted
            if(tempList.size > 0){
                println(tempList)
                val seque = isConsecutive(tempList.map(_.toString.toInt).toArray)
                if (seque == false){
                    println("opa")
                    //return false
                }
            }
            temp = new ListBuffer[String]()
        }
      }
    }
  }

  def solve(row: Int, col: Int): Boolean = {
    if (boardSolved()) {
      return true
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