package edu.ucsb.cs.cs162.regex.vm.compiler

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._

class CompileSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "compile"

  it should "correctly compile the empty language" in { pending }

  it should "correctly compile ε" in { pending }

  it should "correctly compile concatenation" in  { pending }

  it should "correctly compile union" in  { pending }

  it should "correctly compile kleene star" in  { pending }
  // more tests...

  it should "correctly compile complex regexes 1" in {
    val b = Chars('b')
    val bSet = b.chars
    val c = Chars('c')
    val cSet = c.chars
    val regex = Concatenate(Union(b, c), b)
    val instructions = IndexedSeq(
      Fork(1, 5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(3),
      MatchSet(cSet),
      PushChar,
      PushRight,
      MatchSet(bSet),
      PushChar,
      PushConcat,
      Accept
      )

    Compiler.compile(regex) shouldEqual instructions
  }

  it should "correctly compile complex regexes 2" in { pending }

  // more tests...
}
