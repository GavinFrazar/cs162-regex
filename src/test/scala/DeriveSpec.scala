package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class DeriveSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val re_set = "acdb".charset
  val d = Chars('d')
  val b = Chars('b')
  val c = Chars('c')
  val re1 = "ab".charset
  val re2 = "cd".charset.*

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "matches"

  it should "recognize strings that are in a set" in {
    (Derive.matches(re_set, "a")
       && Derive.matches(re_set, "b")
       && Derive.matches(re_set, "c")
       && Derive.matches(re_set, "d")
    ) should equal (true)
  }

  it should "recognize strings of concatenated letters" in {
    Derive.matches("abcd".concatenate, "abcd") should equal (true)
  }

  it should "recognize strings that are in the Union of re1 and re2" in {
    val r = re1 | re2
    (Derive.matches(r, "a")
       && Derive.matches(r, "b")
       && Derive.matches(r, "ccccd")
       && Derive.matches(r, "dddd")
    ) should equal (true) 
  }
  
  it should "recognize strings in Kleenestar(b)" in {
    (Derive.matches(b.*, "bbbbbbbbbb")
       && Derive.matches(b.*, "")
    ) should be (true)
  }

  it should "recognize the empty string" in {
    Derive.matches(ε, "") should be (true)
  }

  it should "recognize strings in re1 ~ re2" in {
    Derive.matches(re1 ~ re2, "ad") should be (true)
  }
  it should "recognize strings that are in the complement of re2" in {
    Derive.matches(!re2, "zyxwutq") should be (true)
  }

  it should "recognize strings that are in re_set & re2" in {
    (Derive.matches(re_set & re2, "c")
       && Derive.matches(re_set & re2, "d")
    ) should be (true)
  }

  // more tests...

  it should "not recognize strings not in the language 1" in {
    Derive.matches(b, "z") should be (false)
  }

  it should "not recognize strings not in the language 2" in {
    Derive.matches(b ~ c, "b") should be (false)
  }

  it should "not recognize strings not in the language 3" in {
    Derive.matches(b | c, "e") should be (false)
  }

  it should "not recognize strings not in the language 4" in {
    Derive.matches(!b, "b") should be (false)
  }

  it should "not recognize strings not in the language 5" in {
    Derive.matches(c.* & c, "") should be (false)
  }

  // more tests...
}
