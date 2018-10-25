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
    ) should equal (true)
  }

  it should "recognize the empty string" in {
    Derive.matches(ε, "") should equal (true)
  }

  it should "recognize strings in re1 ~ re2" in {
    Derive.matches(re1 ~ re2, "ad") should equal (true)
  }
  it should "recognize strings that are in the complement of re2" in {
    Derive.matches(!re2, "zyxwutq") should equal (true)
  }

  it should "recognize strings that are in re_set & re2" in {
    (Derive.matches(re_set & re2, "c")
       && Derive.matches(re_set & re2, "d")
    ) should equal (true)
  }

  // more tests...

  it should "not recognize strings not in the language 1" in {
    Derive.matches(b, "z") should equal (false)
  }

  it should "not recognize strings not in the language 2" in {
    Derive.matches(b ~ c, "b") should equal (false)
  }

  it should "not recognize strings not in the language 3" in {
    Derive.matches(b | c, "e") should equal (false)
  }

  it should "not recognize strings not in the language 4" in {
    Derive.matches(!b, "b") should equal (false)
  }

  it should "not recognize strings not in the language 5" in {
    Derive.matches(c.* & c, "") should equal (false)
  }

  // more tests...

  import edu.ucsb.cs.cs162.regex.derivative._

  behavior of "eval"


  it should "recognize strings that are in a set" in {
    (DerivativeMachine(re_set).eval("a")
       && DerivativeMachine(re_set).eval("b")
       && DerivativeMachine(re_set).eval("c")
       && DerivativeMachine(re_set).eval("d")
    ) should equal (true)
  }

  it should "recognize strings of concatenated letters" in {
    DerivativeMachine("abcd".concatenate).eval("abcd") should equal (true)
  }

  it should "recognize strings that are in the Union of re1 and re2" in {
    val r = re1 | re2
    (DerivativeMachine(r).eval("a")
       && DerivativeMachine(r).eval("b")
       && DerivativeMachine(r).eval("ccccd")
       && DerivativeMachine(r).eval("dddd")
    ) should equal (true)
  }
  
  it should "recognize strings in Kleenestar(b)" in {
    (DerivativeMachine(b.*).eval("bbbbbbbbbb")
       && DerivativeMachine(b.*).eval("")
    ) should equal (true)
  }

  it should "recognize the empty string" in {
    (DerivativeMachine(ε).eval("")) should equal (true)
  }

  it should "recognize strings in re1 ~ re2" in {
    (DerivativeMachine(re1 ~ re2).eval("ad")) should equal (true)
  }
  it should "recognize strings that are in the complement of re2" in {
    (DerivativeMachine(!re2).eval("zyxwutq")) should equal (true)
  }

  it should "recognize strings that are in re_set & re2" in {
    (DerivativeMachine(re_set & re2).eval("c")
       && DerivativeMachine(re_set & re2).eval("d")
    ) should equal (true)
  }

  // more tests...

  it should "not recognize strings not in the language 1" in {
    (DerivativeMachine(b).eval("z")) should equal (false)
  }

  it should "not recognize strings not in the language 2" in {
    (DerivativeMachine(b ~ c).eval("b")) should equal (false)
  }

  it should "not recognize strings not in the language 3" in {
    (DerivativeMachine(b | c).eval("e")) should equal (false)
  }

  it should "not recognize strings not in the language 4" in {
    (DerivativeMachine(!b).eval("b")) should equal (false)
  }

  it should "not recognize strings not in the language 5" in {
    (DerivativeMachine(c.* & c).eval("")) should equal (false)
  }

}
