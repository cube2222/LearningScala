/**
  * Created by jakub on 1/2/2016.
  */
import LearningScala.datastructures
import LearningScala.datastructures.{Leaf, Branch, Cons, Tree}
import LearningScala.errorhandling

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

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

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

  def compose[A,B,C](f: B => C,g: A => B): A => C =
    (a: A) => f(g(a))

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(index: Int): Boolean = {
      if (index >= as.length - 1) true
      else if (!ordered(as(index),as(index+1))) false
      else go(index + 1)
    }
    go(0)
  }

  def main(args: Array[String]): Unit = {
    val x = errorhandling.Some(5)
    val y = errorhandling.None

    def MakeOne(x: Int) = 1
    println(x.map(MakeOne))
    println(y.map(MakeOne))
  }
}
