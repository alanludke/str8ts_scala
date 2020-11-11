// Importação de bibliotecas auxiliares
import scala.io.Source
import scala.collection.mutable.ListBuffer

// Definição do escopo do programa
object Straights {
  var size = 8  // Determina a dimensão do tabuleiro  
  val board: Array[Array[Int]] = Array.ofDim[Int](size, size)  // Variável que representa o tabuleiro principal do Str8ts
  val gaps: Array[Array[Int]] = Array.ofDim[Int](size, size)  // Variável que representa as cores das células do tabuleiro

  // Método principal da implementação onde os tabuleiros
  // são populados e o início do jogo é feito.
  def main(args: Array[String]): Unit = {
    val board_source = readFile("input/board_"+size)
    val gaps_source = readFile("input/gaps_"+size)
    parseBoard(board_source)
    parseGaps(gaps_source)
    solve(0, 0)
  }

  // Realiza a impressão do tabuleiro principal no terminal do usuário.
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
  
  // Método que realiza a leitura do texto de um arquivo externo (board ou gaps).
  def readFile(filename: String): Array[String] = {
    Source.fromFile(filename).getLines.toArray
  }

  // Recebe o array que representa o tabuleiro principal vazio e,
  // utilizando o argumento de input, percorre e preenche os valores adequadamente.
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

  // Recebe o array que representa o tabuleiro auxiliar vazio e,
  // utilizando o argumento de input, percorre e preenche os valores adequadamente.
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

  // Verifica se o número o qual se deseja inserir na célula está presente
  // na linha e na coluna.
  def validate(row: Int, col: Int, num: Int): Boolean = {
    for (i <- 0 until size) {
      if (board(row)(i) == num || board(i)(col) == num) {
        return false
      }
    }
    true
  } 
  
  // Percorre todo o tabuleiro principal em busca de um valor não preenchido(0),
  // ou seja, verifica se todos os campos do tabuleiro ja foram solucionados.
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

  // Método que calcula o valor da linha e da coluna da próxima célula passando-o como parâmetro
  // para o método solve.
  def next(row: Int, col: Int): Boolean = {
    if (col >= (size-1) && (row >= (size-1))) {
      solve(row, col)
    } else if (col >= (size-1)) {
      solve(row + 1, 0)
    } else {
      solve(row, col + 1)
    }
  }

  // Verifica se a sublista de inteiros vinda como parâmetro forma uma sequência de valores válida,
  // ou seja, sem repetição, e todos os números da sequência são iguais ao número anterior + 1.
  def isConsecutive(tempList: Array[Int]): Boolean = {
    for (i <- 0 until (tempList.size - 1)) {
      if (tempList(i+1) != tempList(i)+1) {
        return false
      }
    }
    return true
  }

  // Percorre as colunas do tabuleiro criando listas temporárias delimitadas pelos campos pretos. 
  // Em seguida, chama a função isConsecutive para verificar se os números em cada uma das listas
  // estão em sequência.
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

  // Percorre as linhas do tabuleiro criando listas temporárias delimitadas pelos campos pretos. 
  // Em seguida, chama a função isConsecutive para verificar se os números em cada uma das listas
  // estão em sequência.
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

  // Método que realiza a solução do jogo utilizando a técnica de backtracking.
  // Primeiramente verifica se estamos no final do tabuleiro e se ele está todo preenchido.
  // Nesse caso, chama-se os métodos verifySequenceCol e verifySequenceRow.
  // Se todas as linhas e colunas estiverem em sequência,
  // então o código imprime a solução e encerra a execução.
  // Se não, verifica se a célula que está sendo analisada é diferente de zero e então
  // chama a próxima célula a ser analisada através da função next. 
  // Do contrário, entra no ramo else, onde ele irá executar um laço for que verifica se é possível
  // inserir números na célula em questão. O código tenta inserir todos os números (de 1 até o tamanho do tabuleiro)
  // Se não há nenhum número igual na linha e 
  // na coluna que esse número está sendo inserido, então realiza a inserção.
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