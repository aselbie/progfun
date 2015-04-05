package recfun
import common._

object Main {
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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(balance: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty || balance < 0) balance == 0
      else chars.head match {
        case '(' => loop(balance + 1, chars.tail)
        case ')' => loop(balance - 1, chars.tail)
        case _ => loop(balance, chars.tail)
      }

    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else (0 to money / coins.head).fold(0)(
      (memo, x) => memo + countChange(money - x * coins.head, coins.tail)
    )
}
