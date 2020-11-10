import scala.io.Source
import scala.collection.mutable.ListBuffer

object Straights {
  //Board 5x5: https://wernie-de.de/Stradoku/Stradoku5x5.pdf  -- É o primeiro board disponível
  //Board 6x6: https://www.janko.at/Raetsel/Straights/105.a.htm
  //Board 7x7: https://wernie-de.de/Stradoku/Stradoku7x7.pdf  -- É o primeiro board disponível

  var size = 5
  val board: Array[Array[Int]] = Array.ofDim[Int](size, size)
  val gaps: Array[Array[Int]] = Array.ofDim[Int](size, size)

  def main(args: Array[String]): Unit = {
    val board_source = readFile("input/board_"+size)
    val gaps_source = readFile("input/gaps_"+size)
    parseBoard(board_source)
    parseGaps(gaps_source)
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
    if (col >= (size-1) && (row >= (size-1))) {
      solve(row, col)
    } else if (col >= (size-1)) {
      solve(row + 1, 0)
    } else {
      solve(row, col + 1)
    }
  }

  def isConsecutive(tempList: Array[Int]): Boolean = {
    for (i <- 0 until (tempList.size - 1)) {
      if (tempList(i+1) != tempList(i)+1) {
        return false
      }
    }
    return true
}

  def verifySequenceCol(): Boolean = {
    var temp = new ListBuffer[String]()
    for (j <- 0 until (size)) {
      for (i <- 0 until (size)) {
        if (gaps(i)(j) == 1) {
            temp += (board(i)(j)).toString
            if (i == size-1) {
                val tempList = (temp.toList).sorted
                val seque = isConsecutive(tempList.map(_.toString.toInt).toArray)
                if (seque == false) {
                    return false
                }
                temp = new ListBuffer[String]()
            }
        } else {
            val tempList = (temp.toList).sorted
            if(tempList.size > 0){
                val seque = isConsecutive(tempList.map(_.toString.toInt).toArray)
                if (seque == false){
                    return false
                }
            }
            temp = new ListBuffer[String]()
        }
      }
    }
    return true
  }

  def verifySequenceRow(): Boolean = {
    var temp = new ListBuffer[String]()
    for (i <- 0 until (size)) {
      for (j <- 0 until (size)) {
        if (gaps(i)(j) == 1) {
            temp += (board(i)(j)).toString
            if (j == size-1) {
                val tempList = (temp.toList).sorted
                val seque = isConsecutive(tempList.map(_.toString.toInt).toArray)
                if (seque == false) {
                    return false
                }
                temp = new ListBuffer[String]()
            }
        } else {
            val tempList = (temp.toList).sorted
            if(tempList.size > 0){
                val seque = isConsecutive(tempList.map(_.toString.toInt).toArray)
                if (seque == false){
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
    if (row == size-1 && col == size-1 && boardSolved()) {
      if(verifySequenceCol() && verifySequenceRow()){
        display_board()
        return true
      }
    } else if (board(row)(col) != 0) {
      return next(row, col)
    } else {
      for (i <- 1 to size) {
        if (validate(row, col, i)) {
          board(row)(col) = i;
          if(next(row, col)) {
            return true
          }
          board(row)(col) = 0
        }
      }
    }
    false
  }
}