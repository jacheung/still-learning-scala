package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c==0 || c == r || r == 0 ) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(score: Int, chars: List[Char]): Int = {
      if (chars.isEmpty || score < 0)
        score
      else if (chars.head == '(')
        check(score + 1, chars.tail)
      else if (chars.head == ')')
        check(score - 1, chars.tail)
      else check(score, chars.tail)
    }
    0 == check(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def BreakerOfChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty ) 0
      else if (money == 0 ) 1
      else BreakerOfChange(money, coins.tail) + BreakerOfChange(money - coins.head, coins)
    }
    BreakerOfChange(money, coins)
  }
}
