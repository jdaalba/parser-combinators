package com.jdaalba.core

import com.jdaalba.core.Parsers.ParserOps.{skipL, skipR}
import scalaz.Functor

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}

object Parsers {

  type ParseResult[+A] = Option[(A, String)]
  type Parser[+A] = String => ParseResult[A]

  def parseChar(c: Char): Parser[Char] = parseString(c toString) map (_ charAt 0)

  def parseString(s: String): Parser[String] = i => if (i startsWith s) Some(s, i substring s.length) else None

  def parseWhile(f: String => Boolean): Parser[String] = i => {
    @tailrec
    def g(st: String, tl: String): (String, String) = if (tl == "") {
      (st, tl)
    } else if (f(st)) {
      g(s"$st${tl charAt 0}", tl.tail)
    } else {
      (st.substring(0, st.length - 1), st.substring(st.length - 1) + tl)
    }


    g(i.substring(0, 1), i.tail) match {
      case ("", _) => None
      case (s, t) => Some(s, t)
    }
  }

  def between(s: String): Parser[String] = {

    @tailrec
    def f(tl: String, st: String = ""): ParseResult[String] =
      (tl, (s + "(.*)" + s).r findFirstMatchIn st) match {
        case (t, None) => f(t.tail, st + t.head)
        case (t, Some(m)) => Some(m group 1, t)
        case ("", _) => None
      }

    f(_)
  }

  // implicit constructors
  implicit def char(c: Char): Parser[Char] = parseChar(c)

  implicit def token(s: String): Parser[String] = parseString(s)

  // syntax sugar
  implicit def asParserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = ParserOps.map(p)(f)

    def |(p2: Parser[A]): Parser[A] = ParserOps.or(p, p2)

    def */>[B](pl: Parser[B]): Parser[B] = skipL(p)(pl)

    def <\*(pr: Parser[Any]): Parser[Any] = skipR(p)(pr)
  }

  // parser as Functor
  object ParserOps extends Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      fa andThen (r => r map (t => (f(t._1), t._2)))

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = s => p1(s).orElse(p2(s))

    def skipL[A](p1: Parser[Any])(p2: Parser[A]): Parser[A] = p1(_) flatMap { case (_, s) => p2(s) }

    def skipR[A](p1: Parser[A])(p2: Parser[Any]): Parser[A] =
      p1(_) flatMap { case (r, s) => p2(s) flatMap { case (_, s2) => Some(r, s2) } }
  }
}
