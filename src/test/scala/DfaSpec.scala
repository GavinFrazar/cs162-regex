package edu.ucsb.cs.cs162.dfa

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex.derivative.DerivativeAnalysis._

class DfaSpec extends FlatSpec with Matchers with OptionValues {
  import Regex._

  behavior of "Dfa.getString"

  it should "return None if the DFA's language is empty 1" in {
    val δ = Map(∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, ∅, Set.empty)
    dfa.getString shouldEqual None
  }

  // more tests...

  it should "return a string that the DFA recognizes if the DFA's language is not empty 1" in {
    val δ: Transitions[Regex] = Map(ε → Seq(!CharSet() → ∅), ∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, ε, Set[Regex](ε))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return the only string recognized by aabbcc" in {
    val dfa = analyze("aabbcc".concatenate)
    dfa.getString shouldEqual Some("aabbcc")
  }

  // more tests...
}
