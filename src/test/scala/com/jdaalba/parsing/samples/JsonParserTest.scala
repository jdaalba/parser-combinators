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

  "JsonParser" should "parse an array token" in {
    assertResult(Some((JArray(List(JNull)), "")))(JsonParser("[null]"))
    assertResult(Some((JArray(List(JBoolean(true))), "")))(JsonParser("[true]"))
    assertResult(Some((JArray(List(JString("foo"), JString("bar"))), "")))(JsonParser("[\"foo\",\"bar\"]"))
    assertResult(Some((JArray(List(JArray(List(JString("foo"))))), "")))(JsonParser("[[\"foo\"]]"))
    assertResult(Some((JArray(List(JArray(List(JString("foo"))), JArray(List(JBoolean(true), JBoolean(false))))), "")))(JsonParser("[[\"foo\"],[true,false]]"))
  }
}
