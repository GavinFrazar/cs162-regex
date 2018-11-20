package edu.ucsb.cs.cs162.regex.vm

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class PowersetVmSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._
  import compiler.Compiler
  import parse_tree._

  val b = Chars('b')
  val bSet = b.chars
  val c = Chars('c')
  val cSet = c.chars

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "eval"

  // Replace {language 1} with a more descriptive name for what you're testing.
  // Feel free to add more tests, or write many shorter ones.

  //this is an integration test
  it should "parse username, day, month, and year from example data entry" in {

    val username = (Chars('a'->'z').+).capture("username")
    val date = ((Chars('0'->'9') <= 2).capture("day") ~ Chars('/', '-') ~
                  (Chars('0'->'9') <= 2).capture("month") ~ Chars('/', '-') ~
                  (Chars('0'->'9') ^ 4).capture("year")).capture("date")
    val row = username ~ Chars(',') ~ date

    val program = Compiler.compile(row)
    val tree = (new PowersetVm(program)).eval("tom,25/5/2002").get
    val extractor = (new Extractor(tree))
    extractor.extract("date", "day") shouldEqual List("25")
    extractor.extract("date", "month") shouldEqual List("5")
    extractor.extract("date", "year") shouldEqual List("2002")
  }

  //---------------------------------------------------------------------------
  //unit tests
  //---------------------------------------------------------------------------

  it should "parse the empty string" in {
    val str = ""
    val program = IndexedSeq(PushEmpty, Accept)
    val tree = (new PowersetVm(program)).eval(str).get
    tree should equal (EmptyLeaf)
  }

  it should "parse strings in the language b" in {
    val str = "b"
    val program = IndexedSeq(
      MatchSet(Chars('b').chars),
      PushChar,
      Accept
    )
    val tree = (new PowersetVm(program)).eval(str).get
    tree should equal (CharLeaf('b'))
  }

  it should "parse strings in the language b~c" in {
    val str = "bc"
    val program = IndexedSeq(
      MatchSet(bSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      PushConcat,
      Accept
    )
    val tree = (new PowersetVm(program)).eval(str).get
    tree should equal (ConcatNode(CharLeaf('b'), CharLeaf('c')))
  }

  it should "parse strings in the language b | c" in {
    val str1 = "b"
    val str2 = "c"
    val program = IndexedSeq(
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(cSet),
      PushChar,
      PushRight,
      Accept
    )
    val vm = new PowersetVm(program)
    val tree1 = vm.eval(str1).get
    val tree2 = vm.eval(str2).get
    tree1 should equal (LeftNode(CharLeaf('b')))
    tree2 should equal (RightNode(CharLeaf('c')))
  }

  it should "parse strings in the language b*" in {
    val str1 = ""
    val str2 = "b"
    val str3 = "bbbbbbbbb"
    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      Accept
    )
    val vm = new PowersetVm(program)
    val tree1 = vm.eval(str1).get
    val tree2 = vm.eval(str2).get
    val tree3 = vm.eval(str3).get
    tree1 should equal(StarNode(Seq()))
    tree2 should equal(StarNode(Seq(CharLeaf('b'))))
    tree3 should equal(
      StarNode(
        Seq(
          CharLeaf('b'),
          CharLeaf('b'),
          CharLeaf('b'),
          CharLeaf('b'),
          CharLeaf('b'),
          CharLeaf('b'),
          CharLeaf('b'),
          CharLeaf('b'),
          CharLeaf('b'),
          )
      )
    )
  }

  it should "parse strings in the language b.* ~ (c | d).*" in {
    val str = "bbccdc"
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
    val tree = (new PowersetVm(program)).eval(str).get
    tree should equal(
      ConcatNode(
        StarNode(
          Seq(
            CharLeaf('b'),
            CharLeaf('b')
          )
        ),
        StarNode(
          Seq(
            CharLeaf('c'),
            CharLeaf('c'),
            CharLeaf('d'),
            CharLeaf('c')
          ).reverse
        )
      )
    )
  }

  it should "parse strings in the language Union(b.*, ε).*" in {
    val str1 = ""
    val str2 = "b"
    val str3 = "bb"
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
    val vm = new PowersetVm(program)
    val tree1 = vm.eval(str1).get
    val tree2 = vm.eval(str2).get
    val tree3 = vm.eval(str3).get
    tree1 should equal(StarNode(Seq()))
    tree2 should equal(
      StarNode(
        Seq(
          LeftNode(StarNode(Seq(CharLeaf('b'))))
        )
      )
    )
    tree3 should equal(
      StarNode(
        Seq(
          LeftNode(StarNode(Seq(CharLeaf('b'), CharLeaf('b'))))
        )
      )
    )
  }

  // more tests...

  it should "not parse strings not in the empty-word language" in {
    val str = "a"
    val program = IndexedSeq(PushEmpty, Accept)
    val tree = (new PowersetVm(program)).eval(str)
    tree should equal (None)
  }

  it should "not parse strings not in the language b" in {
    val str = "bb"
    val program = IndexedSeq(
      MatchSet(Chars('b').chars),
      PushChar,
      Accept
    )
    val tree = (new PowersetVm(program)).eval(str)
    tree should equal (None) 
  }

  it should "not parse strings not in the language b~c" in {
    val str = "cb"
    val program = IndexedSeq(
      MatchSet(bSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      PushConcat,
      Accept
    )
    val vm = new PowersetVm(program)
    val tree = vm.eval(str)
    tree shouldEqual None
  }

  it should "not parse strings not in the language b | c" in {
    val str1 = "bc"
    val str2 = "a"
    val program = IndexedSeq(
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(cSet),
      PushChar,
      PushRight,
      Accept
    )
    val vm = new PowersetVm(program)
    val tree1 = vm.eval(str1)
    val tree2 = vm.eval(str2)
    tree1 shouldEqual None
    tree2 shouldEqual None
  }

  it should "not parse strings not in the language b*" in {
    val str1 = "a"
    val str2 = "bc"
    val str3 = "bbbbbbbbbd"
    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      Accept
    )
    val vm = new PowersetVm(program)
    val tree1 = vm.eval(str1)
    val tree2 = vm.eval(str2)
    val tree3 = vm.eval(str3)
    tree1 shouldEqual None
    tree2 shouldEqual None
    tree3 shouldEqual None
  }

  it should "not parse strings not in the language b.* ~ (c | d).*" in {
    val str = "ccdcb"
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
    val vm = new PowersetVm(program)
    val tree = vm.eval(str)
    tree shouldEqual None
  }

  it should "not parse strings not in the language Union(b.*, ε).*" in {
    val str1 = "c"
    val str2 = "bd"
    val str3 = "bbe"
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
    val vm = new PowersetVm(program)
    val tree1 = vm.eval(str1)
    val tree2 = vm.eval(str2)
    val tree3 = vm.eval(str3)
    tree1 shouldEqual None
    tree2 shouldEqual None
    tree3 shouldEqual None
  }
  // more tests...
}
