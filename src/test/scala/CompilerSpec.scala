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

  val b = Chars('b')
  val bSet = b.chars
  val c = Chars('c')
  val cSet = c.chars

  behavior of "compile"

  it should "correctly compile the empty language" in {
    val regex = ∅
    val program = IndexedSeq(
      Reject,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }

  it should "correctly compile ε" in {
    val regex = ε
    val program = IndexedSeq(
      PushEmpty,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }

  it should "correctly compile concatenation" in  {
    val regex = b ~ c
    val program = IndexedSeq(
      MatchSet(bSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      PushConcat,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }

  it should "correctly compile union" in  {
    val regex = (b | c)
    val program = IndexedSeq(
      MatchSet(bSet ++ cSet),
      PushChar,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }

  it should "correctly compile kleene star" in  {
    val regex = b.*
    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }
  // more tests...

  it should "correctly compile complex regexes 1" in {
    val regex = Concatenate(Union(b, c), b)
    val instructions = IndexedSeq(
      Fork(1, 5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(4),
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

  it should "correctly compile complex regexes 2" in {
    val regex = (ε|b).*
    val program = IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1, 10),
      Fork(1, 4),
      PushEmpty,
      PushLeft,
      Jump(4),
      MatchSet(bSet),
      PushChar,
      PushRight,
      PushStar,
      Jump(-10),
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }

  it should "correctly compile complex regexes 3" in {
    val regex = b.* ~ c.*
    val program = IndexedSeq(

    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 4" in {
    val regex = (b.* | c) ~ c.+
    val program = IndexedSeq(

    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 5" in {
    val regex = (!(b | c)).*
    val program = IndexedSeq(

    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 6" in {
    val regex = (b|c).* & (b ~ c.*)
    val program = IndexedSeq(

    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 7" in {
    val regex = (b^(3)) ~ (c^(4)) | (b ~ c)
    val program = IndexedSeq(

    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 8" in {
    val regex = (b.? | c).* 
    val program = IndexedSeq(

    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 9" in {
    val regex = Union(∅, c)
    val program = IndexedSeq(

    )
    Compiler.compile(regex) shouldEqual program
  }

  // more tests...
}
