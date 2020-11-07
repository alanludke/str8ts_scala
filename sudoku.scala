import scala.io.Source

object Sudoku {
  var size = 8
  val puzzle: Array[Array[Int]] = Array.ofDim[Int](size, size)

  def main(args: Array[String]): Unit = {
    val source = readFile("input/board_"+size)
    parsePuzzle(source)
    solve(0, 0)
    display()
  }

  def display(): Unit = {
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        print(puzzle(i)(j) + " ")
      }
      println()
    }
    println()
  }

  def readFile(filename: String): Array[String] = {
    Source.fromFile(filename).getLines.toArray
  }

  def parsePuzzle(puzzleInput: Array[String]): Unit = {
    var row = 0
    var col = 0

    for (line <- puzzleInput) {
      for (c<- line) {
        puzzle(row)(col) = c.asDigit
        col += 1
      }
      row += 1
      col = 0
    }
  }

  def validate(row: Int, col: Int, num: Int): Boolean = {
    for (i <- 0 until size) {
      if (puzzle(row)(i) == num || puzzle(i)(col) == num) {
        return false
      }
    }
    true
  }

  def puzzleSolved(): Boolean = {
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        if (puzzle(i)(j) == 0) {
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

  def solve(row: Int, col: Int): Boolean = {
    if (puzzleSolved()) {
      return true
    } else if (puzzle(row)(col) != 0) {
      return next(row, col)
    } else {
      for (i <- 1 to size) {
        if (validate(row, col, i)) {
          puzzle(row)(col) = i;
          if (next(row, col)) {
            return true
          }
          puzzle(row)(col) = 0
        }
      }
    }
    false
  }
}