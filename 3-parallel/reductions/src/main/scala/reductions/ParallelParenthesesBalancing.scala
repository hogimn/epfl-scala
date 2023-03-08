package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    chars(0) = '('
    chars(1) = '('
    chars(9999) = ')'
    chars(100000) = '('
    chars(90000000) = ')'
    chars(99999999) = ')'
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    @tailrec
    def traverse(idx: Int, acc: Int): Int =
      if idx == chars.length || acc < 0 then acc
      else if (chars(idx) == '(') traverse(idx + 1, acc + 1)
      else if (chars(idx) == ')') traverse(idx + 1, acc - 1)
      else traverse(idx + 1, acc)

    traverse(0, 0) == 0


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) =
      if idx >= until then (arg1, arg2)
      else
        chars(idx) match
          case '(' =>
            traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' =>
            if arg1 > 0 then traverse(idx + 1, until, arg1 - 1, arg2)
            else traverse(idx + 1, until, arg1, arg2 + 1)
          case _ => traverse(idx + 1, until, arg1, arg2)

    def reduce(from: Int, until: Int): (Int, Int) =
      if until - from <= threshold then traverse(from, until, 0, 0)
      else
        val mid = (until - from) / 2 + from
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        (left._1 + right._1 - right._2, left._2)

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

