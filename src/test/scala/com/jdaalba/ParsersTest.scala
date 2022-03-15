package com.jdaalba

import org.scalatest.flatspec.AnyFlatSpec

class ParsersTest extends AnyFlatSpec {

  import com.jdaalba.Parsers._

  "parseChar" should "parse a character" in {
    assertResult(None)(parseChar('c')("abbra"))
    assertResult(Some(('a', "bbra")))(parseChar('a')("abbra"))
  }
}