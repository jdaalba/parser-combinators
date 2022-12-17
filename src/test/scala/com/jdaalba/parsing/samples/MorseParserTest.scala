package com.jdaalba.parsing.samples

import org.scalatest.flatspec.AnyFlatSpec

import scala.language.{implicitConversions, postfixOps}

class MorseParserTest extends AnyFlatSpec {

  for ((code, letter) <- MorseParser.codes) {
    code should s"""be parsed as "$letter"""" in {
      assertResult(Some((letter toString, "")))(MorseParser $ code)
    }
  }

  "Single word" should "be parsed right" in {
    assertResult(Some(("HELLO", "")))(MorseParser $ ".... . .-.. .-.. ---")
  }

  "Sentences" should "be parsed right" in {
    assertResult(Some(("HELLO WORLD", "")))(MorseParser $ ".... . .-.. .-.. ---   .-- --- .-. .-.. -..")
  }
}
