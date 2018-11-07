package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.util._
import edu.ucsb.cs.cs162.range_set._


class DerivativeAnalysisSpec extends FlatSpec with Matchers with Timeout {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._
  import DerivativeAnalysisSpec._

  // The timeout in milliseconds for potentially slow code.
  val timeout = 2000

  // Analyze the given expression subject to a timeout.


  def analyzeWithTimeout(re: Regex) =
    timeoutAfter(timeout) { DerivativeAnalysis.analyze(re) }

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  val charA = Chars('a')
  val charB = Chars('b')
  val charC = Chars('c')
  val re_set = "acdb".charset
  val d = Chars('d')
  val b = Chars('b')
  val c = Chars('c')
  val re1 = "ab".charset
  val re2 = "cd".charset.*
  val Σ = α.chars
  val aSet = CharSet('a')
  val bSet = CharSet('b')
  val cSet = CharSet('c')

  behavior of "the analysis"

  it should "should always terminate 1" in {
    // Causes a timeout or stack overflow if expression similarity isn't
    // implemented correctly.
    val dfa = analyzeWithTimeout((charA | (charA ~ charA)).*)
  }

  it should "should always terminate 2" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.
    val dfa = analyzeWithTimeout(("aa".concatenate | "aaaa".concatenate).*)
  }

  it should "should always terminate 3" in{
    val dfa = analyzeWithTimeout(("a".charset.* | "b".charset.*).*)
  }

  // more tests...

  it should "produce a DFA that recognizes the strings in language 1" in {
    val dfa = analyzeWithTimeout(ε | charA)
    dfa.matches("") should equal (true)
    dfa.matches("a") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 2" in {
    val dfa = analyzeWithTimeout((charA ~ charB | charA ~ charB.*).*)
    dfa.matches("") should equal (true)
    dfa.matches("ab") should equal (true)
    dfa.matches("abbbbbbbbbbbb") should equal (true)
    dfa.matches("aa") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 3" in {
    val re1 = "abc".charset
    val re2 = "cde".charset
    val dfa = analyzeWithTimeout(((re1 & re2) | charA).*)
    dfa.matches("c") should equal (true)
    dfa.matches("a") should equal (true)
    dfa.matches("cacacaaaaaccccacacacacccac") should equal (true)
    dfa.matches("") should equal (true)
  }

  it should "produce a DFA that recognizes strings that are in a set" in {
    val dfa = analyzeWithTimeout(re_set)
    (dfa.matches("a")
       &&
       dfa.matches("b")
       &&
       dfa.matches("c")
       &&
       dfa.matches("d")
    ) should equal (true)
  }

  it should "produce a DFA that recognizes strings of concatenated letters" in {
    val dfa = analyzeWithTimeout("abcd".concatenate)
    dfa.matches("abcd") should equal (true)
  }

  it should "produce a DFA that recognizes strings that are in the Union of re1 and re2" in {
    val r = re1 | re2
    val dfa = analyzeWithTimeout(r)
    (dfa.matches("a")
       &&
       dfa.matches("b")
       &&
       dfa.matches("ccccd")
       &&
       dfa.matches("dddd")
    ) should equal (true)
  }
  
  it should "produce a DFA that recognizes strings in Kleenestar(b)" in {
    val dfa = analyzeWithTimeout(b.*)
    (dfa.matches("bbbbbbbbbb")
       &&
       dfa.matches("")
    ) should equal (true)
  }

  it should "produce a DFA that recognizes the empty string" in {
    val dfa = analyzeWithTimeout(ε)
    (dfa.matches("")) should equal (true)
  }

  it should "produce a DFA that recognizes strings in re1 ~ re2" in {
    val dfa = analyzeWithTimeout(re1 ~ re2)
    (dfa.matches("ad")) should equal (true)
  }
  it should "produce a DFA that recognizes strings that are in the complement of re2" in {
    val dfa = analyzeWithTimeout(!re2)
    (dfa.matches("zyxwutq")) should equal (true)
  }

  it should "produce a DFA that recognizes strings that are in re_set & re2" in {
    val dfa = analyzeWithTimeout(re_set & re2)
    (dfa.matches("c")
       &&
       dfa.matches("d")
    ) should equal (true)
  }

  // more tests...

  it should "produce a DFA that should not recognize strings not in the language 1" in {
    val dfa = analyzeWithTimeout(ε | charA)
    dfa.matches("b") should equal (false)
    dfa.matches("aa") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 2" in {
    val dfa = analyzeWithTimeout(b)
    (dfa.matches("z")) should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 3" in {
    val dfa = analyzeWithTimeout(b ~ c)
    (dfa.matches("b")) should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 4" in {
    val dfa = analyzeWithTimeout(b | c)
    (dfa.matches("e")) should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 5" in {
    val dfa = analyzeWithTimeout(!b)
    (dfa.matches("b")) should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 6" in {
    val dfa = analyzeWithTimeout(c.* & c)
    (dfa.matches("")) should equal (false)
  }


  // more tests...

  it should "produce a DFA that has the correct structure 1" in {
    val re = charA ~ charB
    val dfa = DerivativeAnalysis.analyze(re)
    val states = Set(re, charB, ε, ∅)

    //check initial state
    dfa.init should equal (re)

    //check final states
    dfa.fin should equal (Set[Regex](ε))

    // Check if the transition relation is computed for all states
    dfa.delta.keySet should equal (states)

    // Check the transition relation
    (dfa.delta(re) isEquivalentSeq Seq((!aSet, ∅), (aSet, charB))
       should equal (true))
    (dfa.delta(charB) isEquivalentSeq Seq((!bSet, ∅), (bSet, ε))
        should equal (true))
    (dfa.delta(ε) isEquivalentSeq Seq((Σ, ∅)) should equal (true))
    (dfa.delta(∅) isEquivalentSeq Seq((Σ, ∅)) should equal (true))
  }

  // more tests...

  it should "produce a DFA that has the correct structure 2" in {
    val re = charA | charB
    val dfa = DerivativeAnalysis.analyze(re)
    val states = Set(re, ε, ∅)

    //check initial state
    dfa.init should equal (re)

    //check final states
    dfa.fin should equal (Set[Regex](ε))

    //check if transitions computed for all states
    dfa.delta.keySet should equal (states)

    // Check the transition relation
    (dfa.delta(re) isEquivalentSeq Seq((!(aSet ++ bSet), ∅), (aSet ++ bSet, ε))
       should equal (true))
    (dfa.delta(ε) isEquivalentSeq Seq((Σ, ∅)) should equal (true))
    (dfa.delta(∅) isEquivalentSeq Seq((Σ, ∅)) should equal (true))
  }


  it should "produce a DFA that has the correct structure 3" in {
    val re = "ab".charset & "bc".charset 
    val dfa = DerivativeAnalysis.analyze(re)
    val states = Set(re, ε, ∅)

    //check initial state
    dfa.init should equal (re)

    //check final states
    dfa.fin should equal (Set[Regex](ε))

    //check if transitions computed for all states
    dfa.delta.keySet should equal (states)

    // Check the transition relation
    dfa.delta(re) isEquivalentSeq Seq((bSet, ε), (!bSet, ∅))
    dfa.delta(ε) isEquivalentSeq Seq((Σ, ∅)) should equal (true)
    dfa.delta(∅) isEquivalentSeq Seq((Σ, ∅)) should equal (true)
  }


  it should "produce a DFA that has the correct structure 4" in {
    val re = charA.*
    val dfa = DerivativeAnalysis.analyze(re)
    val states = Set(re, ∅)

    //check initial state
    dfa.init should equal (re)

    //check final states
    dfa.fin should equal (Set[Regex](re))

    //check if transitions computed for all states
    dfa.delta.keySet should equal (states)

    // Check the transition relation
    (dfa.delta(re) isEquivalentSeq Seq((aSet, re), (!aSet, ∅))) shouldEqual true
    (dfa.delta(∅) isEquivalentSeq Seq((Σ, ∅))) should equal (true)
  }


  it should "produce a DFA that has the correct structure 5" in {
    val re = !charA
    val dfa = DerivativeAnalysis.analyze(re)
    val states = Set(re, α ~ α.*, α.*)

    //check initial state
    dfa.init should equal (Complement(charA))

    //check final states
    dfa.fin should equal (Set[Regex](Complement(charA), α.*))

    //check if transitions computed for all states
    dfa.delta.keySet should equal (states)

    // Check the transition relation
    (dfa.delta(Complement(charA)) isEquivalentSeq
       Seq((aSet, α ~ α.*), (!aSet, α.*))) should equal (true)
    dfa.delta(α ~ α.*) isEquivalentSeq Seq((Σ, α.*)) should equal (true)
    dfa.delta(α.*) isEquivalentSeq Seq((Σ, α.*)) should equal (true)
  }


  it should "produce a DFA that has the correct structure 6" in {
    val re = ε
    val dfa = DerivativeAnalysis.analyze(re)
    val states = Set(ε, ∅)

    //check initial state
    dfa.init should equal (ε)

    //check final states
    dfa.fin should equal (Set[Regex](ε))

    //check if transitions computed for all states
    dfa.delta.keySet should equal (states)

    // Check the transition relation
    (dfa.delta(ε) isEquivalentSeq Seq((Σ, ∅))) should equal (true)
    dfa.delta(∅) isEquivalentSeq Seq((Σ, ∅)) should equal (true)
  }


  it should "produce a DFA that has the correct structure 7" in {
    val re = !(charC.*)
    val dfa = DerivativeAnalysis.analyze(re)
    val states = Set(re, α.*)

    //check initial state
    dfa.init should equal (re)

    //check final states
    dfa.fin should equal (Set[Regex](α.*))

    //check if transitions computed for all states
    dfa.delta.keySet should equal (states)

    // Check the transition relation
    (dfa.delta(re) isEquivalentSeq Seq((cSet, re), (!cSet, α.*))) should equal (true)
    (dfa.delta(α.*) isEquivalentSeq Seq((Σ, α.*))) should equal (true)
  }
}


object DerivativeAnalysisSpec {
  implicit class SeqIsSubseq(self: Seq[(CharSet, Regex)]) {
    def isSubseqOf(other: Seq[(CharSet, Regex)]) = {
      (true /: self){
        (acc, curr_elem) => acc && other.contains(curr_elem)
      }
    }
  }


  implicit class SeqEquivalency(self: Seq[(CharSet, Regex)]) {
    def isEquivalentSeq(other: Seq[(CharSet, Regex)]) = {
      val sameLength = self.length == other.length
      (self isSubseqOf other) && (other isSubseqOf self) && sameLength
    }
  }
}
