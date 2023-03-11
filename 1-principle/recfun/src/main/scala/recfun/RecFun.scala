package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r || r == 0 then
      1
    else
      pascal(c - 1, r - 1) + pascal(c    , r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def balanceAcc(chars: List[Char], acc: Int): Int =
      if (chars.isEmpty || acc < 0)
        acc
      else if (chars.head == '(')
        balanceAcc(chars.tail, acc + 1)
      else if (chars.head == ')')
        balanceAcc(chars.tail, acc - 1)
      else
        balanceAcc(chars.tail, acc)

    balanceAcc(chars, 0) == 0

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then
      1
    else if money < 0 || coins.isEmpty then
      0
    else
      countChange(money, coins.tail) +
      countChange(money - coins.head, coins)

