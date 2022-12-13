package com.jdaalba.samples

import org.scalatest.flatspec.AnyFlatSpec

class JsonParserTest extends AnyFlatSpec {

  "parse a number" should "return a JNumber" in {
    assertResult(Some(JNumber(123.45), ""))(JsonParser.parse("123.45"))
  }

  "parse a string" should "return a JString" in {
    assertResult(Some((JString("foo"), "")))(JsonParser.parse("\"foo\""))
  }

  "parse a boolean" should "return a JBoolean" in {
    assertResult(Some((JBoolean(true), "")))(JsonParser.parse("true"))
    assertResult(Some((JBoolean(false), "")))(JsonParser.parse("false"))
  }
}
