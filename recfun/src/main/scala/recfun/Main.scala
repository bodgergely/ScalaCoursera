package recfun

object Main {
  def main(args: Array[String]) {
    println(countChange(4,List(1,2)))
  }

  def printPascal(): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || c == r) 1 else pascal(c-1,r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def _balance(chars: List[Char], num: Int): Boolean = {
          if(chars.isEmpty && num == 0) true
          else if(chars.isEmpty && num != 0) false
          else if(chars.head == ')' && num == 0) false
          else if(chars.head == '(') _balance(chars.tail, num+1)
          else if(chars.head == ')' && num > 0) _balance(chars.tail, num - 1)
          else _balance(chars.tail, num)
      }
      _balance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty && money > 0) 0
      else if(money < 0) 0
      else if(money == 0) 1
      else countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }



}

