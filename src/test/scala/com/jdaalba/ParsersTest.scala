package com.jdaalba

import org.scalatest.flatspec.AnyFlatSpec

class ParsersTest extends AnyFlatSpec {

  import com.jdaalba.Parsers._
  import com.jdaalba.Parsers.ParserOps._

  "parseChar" should "parse a character" in {
    assertResult(None)(parseChar('c')("abbra"))
    assertResult(Some(('a', "bbra")))(parseChar('a')("abbra"))
  }

  "parseString" should "parse a string" in {
    assertResult(Some(("", "abbra")))(parseString("")("abbra"))
    assertResult(None)(parseString("abbra")("cadabbra"))
    assertResult(Some(("abbra", " cadabbra")))(parseString("abbra")("abbra cadabbra"))
  }

  "or" should "choose, if exists, the non failed parser" in {
    val p1: Parser[String] = "abbra"
    val p2: Parser[String] = "cadabbra"
    assertResult(None)((p1 | p2)("none"))
    assertResult(Some("abbra", ""))((p1 | p2)("abbra"))
    assertResult(Some("cadabbra", ""))((p1 | p2)("cadabbra"))
  }
}