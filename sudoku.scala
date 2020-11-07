import scala.io.Source

object Sudoku {
  val puzzle: Array[Array[Int]] = Array.ofDim[Int](9, 9)

  def main(args: Array[String]): Unit = {
    parsePuzzle(readFile("input/board_9.txt"))
    display()
    solve(0, 0)
    display()
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
    for (i <- 0 until 9) {
      if (puzzle(row)(i) == num || puzzle(i)(col) == num) {
        return false
      }
    }

    val r = (row / 3) * 3
    val c = (col / 3) * 3
    for (i <- r until r + 3) {
      for (j <- c until c + 3) {
        if (puzzle(i)(j) == num) {
          return false
        }
      }
    }
    true
  }

  def display(): Unit = {
    for (i <- 0 until 9) {
      for (j <- 0 until 9) {
        print(puzzle(i)(j) + " ")
      }
      println()
    }
    println()
  }

  def puzzleSolved(): Boolean = {
    for (i <- 0 until 9) {
      for (j <- 0 until 9) {
        if (puzzle(i)(j) == 0) {
          return false
        }
      }
    }
    true
  }

  def next(row: Int, col: Int): Boolean = {
    if (col >= 8) {
      solve(row + 1, 0)
    } else {
      solve(row, col + 1)
    }
  }

  def solve(row: Int, col: Int): Boolean = {
    if (puzzleSolved()) {
      return true
    } else if (puzzle(row)(col) > 0) {
      return next(row, col)
    } else {
      for (i <- 1 to 9) {
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