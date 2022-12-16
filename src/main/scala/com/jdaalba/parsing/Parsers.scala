package com.jdaalba.parsing

import scalaz.{Monad, Monoid}

import scala.language.{implicitConversions, postfixOps}

object Parsers {

  type ParseResult[+A] = Option[(A, String)]

  def char(c: Char): Parser[Char] = i =>
    i headOption match {
      case Some(`c`) => (c, i tail)
      case _ => None
    }

  def string: String => Parser[String] = {
    case "" => point("")
    case in => in.head $ _ flatMap {
      case (st1, tl1) => in.tail $ tl1 map {
        case (st2, tl2) => (st1 + st2, tl2)
      }
    }
  }

  def point[A](a: => A): Parser[A] = ParserOps point a

  def matches(f: Char => Boolean): Parser[String] = inp =>
    inp.headOption.filter(f)
      .flatMap { c1 =>
        matches(f)(inp tail).map {
          case (c2, t2) => (c1 + c2, t2)
        }.orElse((c1 toString, inp.tail))
      }

  implicit def toChar: Char => Parser[Char] = char

  implicit def tok: String => Parser[String] = string

  implicit def toParseResult[A](t: (A, String)): ParseResult[A] = Some(t)

  object ParserOps extends Monad[Parser] {

    override def bind[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
      fa $ _ match {
        case Some((a, out)) => f(a) $ out
        case _ => None
      }

    override def point[A](a: => A): Parser[A] = (a, _)
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

    def $: String => ParseResult[A] = apply

    def map[B](f: A => B): Parser[B] = ParserOps.map(self)(f)

    def bind[B](f: A => Parser[B]): Parser[B] = ParserOps.bind(self)(f)

    def <|>[B >: A](pb: Parser[B]): Parser[B] = inp =>
      self $ inp match {
        case None => pb $ inp
        case rb => rb
      }

    def *>[B](p: => Parser[B]): Parser[B] = self(_) flatMap (t => p(t._2))

    def <*(p: => Parser[Any]): Parser[A] = inp =>
      for {
        (a, t1) <- self $ inp
        (_, t2) <- p $ t1
      } yield (a, t2)

    def ?*>[B](p: => Parser[B]): Parser[B] = p <|> (self *> p)

    def <*?(p: => Parser[Any]): Parser[A] = (self <* p) <|> self

    def replaceVal[B](b: B): Parser[B] = self map (_ => b)
  }
}
