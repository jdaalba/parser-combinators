package com.jdaalba.core

import org.scalatest.flatspec.AnyFlatSpec

class ParsersTest extends AnyFlatSpec {

  import com.jdaalba.core.Parsers._

  "parseChar" should "parse a character" in {
    assertResult(None)(parseChar('c')("abbra"))
    assertResult(Some(('a', "bbra")))(parseChar('a')("abbra"))
  }

  "parseString" should "parse a string" in {
    assertResult(Some(("", "abbra")))(parseString("")("abbra"))
    assertResult(None)(parseString("abbra")("cadabbra"))
    assertResult(Some(("abbra", " cadabbra")))(parseString("abbra")("abbra cadabbra"))
    assertResult(Some(("cad", "abbra abbra")))(parseString("cad")("cadabbra abbra"))
  }

  "or" should "choose, if exists, the non failed parser" in {
    val p1: Parser[String] = "abbra"
    val p2: Parser[String] = "cadabbra"
    assertResult(None)((p1 | p2)("none"))
    assertResult(Some("abbra", ""))((p1 | p2)("abbra"))
    assertResult(Some("cadabbra", ""))((p1 | p2)("cadabbra"))
  }

  "skipL" should "skip left parser if matches" in {
    val p1: Parser[String] = "cad"
    val p2: Parser[String] = "abbra"

    assertResult(None)((p1 */> p2)("cadnada"))
    assertResult(None)((p1 */> p2)("abbra"))
    assertResult(Some("abbra", " abbra"))((p1 */> p2)("cadabbra abbra"))
  }

  "skipR" should "skip right parser if matches" in {
    val p1: Parser[String] = "cad"
    val p2: Parser[String] = "abbra"

    assertResult(None)((p1 <\* p2)("cadnada"))
    assertResult(None)((p1 <\* p2)("abbra"))
    assertResult(Some("cad", " abbra"))((p1 <\* p2)("cadabbra abbra"))
  }

  "parseWhile" should "parse while content matches predicate" in {
    assertResult(Some(("123", "foo")))(parseWhile(_ matches "\\d+")("123foo"))
    assertResult(None)(parseWhile(_ matches "\\d+")("foo"))
  }

  "between" should "parse content surrounded by string" in {
    assertResult(Some("bar", ""))(between("foo")("foobarfoo"))
  }
}