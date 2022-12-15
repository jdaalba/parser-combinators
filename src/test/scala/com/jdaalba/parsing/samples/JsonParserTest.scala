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
    assertResult(Some((JArray(JNull), "")))(JsonParser("[null]"))
    assertResult(Some((JArray(JBoolean(true)), "")))(JsonParser("[true]"))
    assertResult(Some((JArray(JString("foo"), JString("bar")), "")))(JsonParser("[\"foo\",\"bar\"]"))
    assertResult(Some((JArray(JArray(JString("foo"))), "")))(JsonParser("[[\"foo\"]]"))
    assertResult(Some((JArray(JArray(JString("foo")), JArray(JBoolean(true), JBoolean(false))), "")))(JsonParser("[[\"foo\"],[true,false]]"))
    assertResult(Some((JArray(JObject("foo" -> JNull)), "")))(JsonParser("""[{"foo":null}]"""))
  }

  "JsonParser" should "parse a JSON object" in {
    assertResult(
      Some((
        JObject(
          "foo" -> JString("val1"),
          "bar" -> JArray(JBoolean(true), JBoolean(false)),
          "baz" -> JObject(
            "e1" -> JNumber(123),
            "e2" -> JNull
          )
        ),
        ""
      ))
    )(JsonParser("""{"foo":"val1","bar":[true,false],"baz":{"e1":123,"e2":null}}"""))
  }
}
