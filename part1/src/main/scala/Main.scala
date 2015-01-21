package sets

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {

    import IntSet._

    System.out.println("!!!")

    /*
    def azaza(i: Int, f:(Int, Int)=>Int):Int = {
      f(i, 2)
    }

    val testSet1 = union(singletonSet(1), singletonSet(2))
    val aa = (x: Int, y:Int) => x * 2

    println(azaza(2, aa))
*/


    println(fib(7))

  }


  def fib(n: Int): Int = {

    @tailrec
    def loop(prev: Int, preprev: Int, n: Int):Int =

      if (n == 0) prev + preprev

      else {
        loop(prev + preprev,  prev , n - 1)
      }
    if (n==1) 0
    else if (n==2) 1
    else {
      loop(1, 0, n-3)
    }

  }
}

object Types {
  type ??? = Nothing
  type *** = Any
}
import Types._