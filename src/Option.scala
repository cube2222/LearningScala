package LearningScala.errorhandling

import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(x) => if(f(x)) Some(x) else None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
