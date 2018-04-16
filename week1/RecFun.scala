package recfun

object RecFun {
  def main(args: Array[String]) {
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
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanceCount(chars: List[Char], openPCount: Int, closePCount: Int): Boolean = {
      if (openPCount < closePCount) false
      else if (openPCount == closePCount && chars.isEmpty) true
      else balanceCount(chars.tail, countParentheses(chars.head, '(', openPCount), countParentheses(chars.head, ')', closePCount))
    }

    def countParentheses(current: Char, p: Char, count: Int): Int = current match {
      case `p` => count + 1
      case _ => count
    }


    balanceCount(chars, 0, 0)

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def total(money: Int,coins: List[Int], i: Int): Int = {
      if (money < 0) return 0
      if (money == 0) return 1
      if (i == coins.length && money > 0) return 0
      total(money - coins(i), coins, i) + total(money, coins, i + 1)
    }

    total(money,coins,0)
  }
}
