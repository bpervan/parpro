package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
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
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def inner(chars: Array[Char], accu: Int): Boolean = {
      if(accu < 0) false
      else if(chars.isEmpty) accu == 0
      else if(chars.head.equals('(')) inner(chars.tail, accu + 1)
      else if(chars.head.equals(')')) inner(chars.tail, accu - 1)
      else inner(chars.tail, accu)
    }

    inner(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    //Sequential
    //Use while or make it tail-recursive (don't use range)
    def traverse(idx: Int, until: Int, delta: Int, depth: Int) /*: ???*/ = {
      ???
    }

    //Parallel
    def reduce(from: Int, until: Int): Boolean /*: ???*/ = {
      if(until - from < threshold){
        //Switch to sequential version
        balance(chars.slice(from, until))
      } else {
        val mid = from + (until - from) / 2
        val (res1, res2) = parallel(reduce(from, mid), reduce(mid, until))
        res1 && res2
      }
      //???
    }

    reduce(0, chars.length)// == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
  // I want more, but I'm not masochistic

}
