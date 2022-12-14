package com.jdaalba.parsing.samples

import org.scalatest.flatspec.AnyFlatSpec

class JsonParserTest extends AnyFlatSpec {

  "JsonParser" should "parse a null token" in {
    assertResult(Some((JNull, "")))(JsonParser("null"))
  }

  "JsonParser" should "parse a boolean token" in {
    assertResult(Some((JBoolean(true), "")))(JsonParser("true"))
    assertResult(Some((JBoolean(false), "")))(JsonParser("false"))
  }

  "JsonParser" should "parse a numeric token" in {
    assertResult(Some((JNumber(123), "")))(JsonParser("123"))
  }

  "JsonParser" should "parse an alphanumeric token" in {
    assertResult(Some((JString("foo"), "")))(JsonParser("\"foo\""))
  }
}
