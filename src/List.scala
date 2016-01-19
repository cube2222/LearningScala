/**
  * Created by Jakub on 2016-01-03.
  */
package LearningScala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def addOneToEach(ns: List[Int]): List[Int] = ns match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOneToEach(xs))
  }

  def doublesToString(ns: List[Double]): List[String] = ns match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString(), doublesToString(xs))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((x,y) => Cons(f(x), y))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concatenate(map(as)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] = (xs,ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a,b), zipWith(as,bs)(f))
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
      def go[A](sup: List[A], sub: List[A], Persistency: List[A]): Boolean = (sup, sub) match {
        case (Cons (x, xs), Cons (y, ys) ) => if (x == y) go(xs, ys, Persistency) else go (xs,Persistency, Persistency)
        case (Nil, _) => false
        case (_, Nil) => true
    }
    go(sup, sub, sub)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0
    case Cons(x, xs) => x * product(xs)
  }

  def foldLeft[A,B](as: List[A], z: B)(f:(B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](ns: List[A]) = {
    foldLeft(ns, 0)((acc,_) => acc + 1)
  }
  def reverse[A](ns: List[A]) = {
    foldLeft(ns, List[A]())((x, y) => Cons(y,x))
  }
  def append[A](xs: List[A],ys: List[A]): List[A] = xs match {
    case Nil => ys
    case Cons(z, zs) => Cons(z,append(zs,ys))
  }
  def concatenate[A](ns: List[List[A]]): List[A] = {
    foldRight2(ns, List[A]())(append)
  }

  def foldRight[A,B](as: List[A],z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldRight2[A,B](as: List[A],z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)((x,y) => f(y,x))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)((x,y) => x * y)

  def length[A](as: List[A]):Int = as match {
    case Nil => 0
    case Cons(x, xs) => 1 + length(xs)
  }

  def tail[A](s: List[A]): List[A] = s match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[A](s: List[A],n: Int): List[A] = {
    def go(gs: List[A],m: Int): List[A] = gs match {
      case Nil => Nil
      case Cons(x, xs) => {
        if(m > 0) go(xs, m-1)
        else Cons(x, xs)
      }
    }
    go(s, n)
  }

  def dropWhile[A](as: List[A])(f: A => Boolean):List[A] = as match{
    case Cons(x,xs) if f(x) => dropWhile(xs)(f)
    case _ => as
  }

  def setHead[A](s: List[A],n: A): List[A] = s match {
    case Nil => Cons(n, Nil)
    case Cons(x, xs) => Cons(n, xs)
  }

  def init[A](l: List[A]): List[A] = {
    def go(s: List[A]): List[A] = s match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, go(xs))
    }
    go(l)
  }

  def apply[A](as: A*): List[A] =
  if(as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))
}


