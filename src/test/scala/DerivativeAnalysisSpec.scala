package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.util._

class DerivativeAnalysisSpec extends FlatSpec with Matchers with Timeout {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  // The timeout in milliseconds for potentially slow code.
  val timeout = 2000

  // Analyze the given expression subject to a timeout.
  def analyzeWithTimeout(re: Regex) =
    timeoutAfter(timeout) { DerivativeAnalysis.analyze(re) }

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "the analysis"

  it should "should always terminate 1" in {
    val charA = Chars('a')

    // Causes a timeout or stack overflow if expression similarity isn't
    // implemented correctly.
    val dfa = analyzeWithTimeout((charA | (charA ~ charA)).*)
  }

  it should "should always terminate 2" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.
    pending
  }

  // more tests...

  it should "produce a DFA that recognizes the strings in language 1" in {
    val charA = Chars('a')

    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("") should equal (true)
    dfa.matches("a") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 2" in {
    pending
  }

  // more tests...

  it should "produce a DFA that should not recognize strings not in the language 1" in {
    val charA = Chars('a')

    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("b") should equal (false)
    dfa.matches("aa") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 2" in {
    pending
  }

  // more tests...

  it should "produce a DFA that has the correct structure 1" in {
    pending
  }

  // more tests...
}
