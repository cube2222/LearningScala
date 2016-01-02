/**
  * Created by jakub on 1/2/2016.
  */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n,1)
  }

  def formatFunction(f: Int => Int, n: Int): String = {
    val msg = "The function of %d is %d"
    msg.format(n, f(n))
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    def tempFunction(a: A): (B => C) = {
      def midTempFunction(b: B): C = {
        f(a,b)
      }
      midTempFunction
    }
    tempFunction
  }

  def Fib(n: Int): Int = {
    @annotation.tailrec
    def go(n1: Int, n2: Int, RepsLeft: Int): Int = {
      if (RepsLeft <= 0) n2
      else go(n2, n1+n2, RepsLeft - 1)
    }
    if(n == 1) 0
    else if (n == 2) 1
    else go(0,1,n-2)
  }
  def main(args: Array[String]): Unit = {

  }
}
