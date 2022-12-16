package com.jdaalba.parsing

import scalaz.{Monad, Monoid}

import scala.language.{implicitConversions, postfixOps}

object Parsers {

  type ParseResult[+A] = Option[(A, String)]

  def char(c: Char): Parser[Char] = i => i headOption match {
    case Some(`c`) => Some((c, i tail))
    case _ => None
  }

  def string(s: String): Parser[String] = i =>
    if (s isEmpty) {
      Some(("", i))
    } else {
      s.head(i) flatMap (t => s.tail(t._2) map (c => (t._1 + c._1, c._2)))
    }

  def point[A](a: => A): Parser[A] = ParserOps point a

  def matches(f: Char => Boolean): Parser[String] = inp => inp.headOption.filter(f)
    .flatMap(c => matches(f)(inp tail).map(t => (c + t._1, t._2)).orElse(Some(c toString, inp.tail)))

  implicit def toChar: Char => Parser[Char] = char

  implicit def tok: String => Parser[String] = string

  object ParserOps extends Monad[Parser] {

    // Parser as Functor
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = fa(_) match {
      case Some((v, out)) => Some((f(v), out))
      case _ => None
    }

    // Parser as Monad
    override def bind[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = fa(_) match {
      case Some((a, out)) => f(a)(out)
      case _ => None
    }

    // Parser as Applicative
    override def point[A](a: => A): Parser[A] = inp => Some((a, inp))
  }

  case class ParserMonoid[A](monoid: Monoid[A]) extends Monoid[Parser[A]] {

    override def zero: Parser[A] = point(monoid zero)

    override def append(f1: Parser[A], f2: => Parser[A]): Parser[A] = inp =>
      for {
        (a1: A, t1: String) <- f1(inp)
        (a2: A, t2: String) <- f2(t1)
      } yield (monoid.append(a1, a2), t2)
  }

  trait Parser[+A] {
    self =>

    def apply(string: String): ParseResult[A]

    def map[B](f: A => B): Parser[B] = ParserOps.map(self)(f)

    def bind[B](f: A => Parser[B]): Parser[B] = ParserOps.bind(self)(f)

    def <|>[B >: A](pb: Parser[B]): Parser[B] = inp => self(inp) match {
      case None => pb(inp)
      case r => r
    }

    def *>[B](p: => Parser[B]): Parser[B] = self(_) flatMap (t => p(t._2))

    def <*(p: => Parser[Any]): Parser[A] = inp => for {
      o1 <- self(inp)
      o2 <- p(o1._2)
    } yield (o1._1, o2._2)

    def ?*>[B](p: => Parser[B]): Parser[B] = p <|> (self *> p)

    def <*?(p: => Parser[Any]): Parser[A] = (self <* p) <|> self

    def replaceVal[B](b: B): Parser[B] = self map (_ => b)
  }
}
