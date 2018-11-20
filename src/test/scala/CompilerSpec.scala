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
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 4" in {
    val regex = (b.* | c) ~ c.+
    val program = IndexedSeq(
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(8),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushRight,
      MatchSet(cSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushConcat,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 5" in {
    val regex = ((b | c)).*
    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bSet ++ cSet),
      PushChar,
      PushStar,
      Jump(-4),
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 6" in {
    val regex = (b^(3)) ~ (c^(4)) | (b ~ c)
    val program = IndexedSeq(
      Fork(1,8),
      MatchSet(bSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      PushConcat,
      PushLeft,
      Jump(22),
      MatchSet(bSet),
      PushChar,
      MatchSet(bSet),
      PushChar,
      MatchSet(bSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      PushConcat,
      PushConcat,
      PushConcat,
      PushConcat,
      PushConcat,
      PushConcat,
      PushRight,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 7" in {
    val regex = (b.? | c).* 
    val program = IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1,16),
      Fork(1,4),
      PushEmpty,
      PushLeft,
      Jump(10),
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(cSet),
      PushChar,
      PushRight,
      PushRight,
      PushStar,
      Jump(-16),
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }
  it should "correctly compile complex regexes 8" in {
    val regex = Union(∅, c)
    val program = IndexedSeq(
      Fork(1,4),
      Reject,
      PushLeft,
      Jump(4),
      MatchSet(cSet),
      PushChar,
      PushRight,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }

  it should "correctly compile complex regexes 9" in {
    val regex = (Union(b.*, ε).*).*
    val program = IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1,14),
      Fork(1,9),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushLeft,
      Jump(3),
      PushEmpty,
      PushRight,
      PushStar,
      Jump(-14),
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }

  it should "correctly compile complex regexes 10" in {
    val regex = b.* ~ (c | Chars('d')).*
    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      InitStar,
      Fork(1,5),
      MatchSet((cSet ++ Chars('d').chars)),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      Accept
    )
    Compiler.compile(regex) shouldEqual program
  }
  // more tests...
}
