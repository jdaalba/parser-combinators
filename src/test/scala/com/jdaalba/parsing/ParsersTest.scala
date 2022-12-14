package com.jdaalba.parsing

import org.scalatest.flatspec.AnyFlatSpec

import scala.language.postfixOps

class ParsersTest extends AnyFlatSpec {

  import com.jdaalba.parsing.Parsers._

  "char" should "create a char parser" in {
    assertResult(None)('a'("123"))
    assertResult(Some(('1', "23")))('1'("123"))
  }

  "string" should "create a string parser" in {
    assertResult(None)("abbra"("cadabbra"))
    assertResult(Some("", ""))(""(""))
    assertResult(Some("abbra", "cadabbra"))("abbra"("abbracadabbra"))
  }

  "Parser" should "be a Functor" in {
    assertResult(Some(('a', "bbra")))('a'.map(identity)("abbra"))
    assertResult(Some(("aa", "bbra")))('a'.map(c => s"$c" repeat 2)("abbra"))
    assertResult(None)('a'.map(identity)("123"))
  }

  "Parser" should "be a Monad" in {
    assertResult(Some((None, "foo")))(point(None)("foo"))
    assertResult(Some(("foo", "bar")))(point("foo")("bar"))
    assertResult(None)('1'.bind(_ toString)("abbra"))
    assertResult(Some("1", "2345"))('1'.bind(_ toString)("112345"))
  }

  "<|>" should "take first if not empty or second" in {
    assertResult(Some('a', "bba"))(('a' <|> 'b') ("abba"))
    assertResult(Some('b', "ba"))(('a' <|> 'b') ("bba"))
  }
}