package com.jdaalba.parsing

import scalaz.Monoid

package object utils {

  object StringMonoid extends Monoid[String] {
    override def zero: String = ""

    override def append(f1: String, f2: => String): String = f1 + f2
  }
}
