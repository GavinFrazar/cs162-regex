package edu.ucsb.cs.cs162.dfa

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex.derivative.DerivativeAnalysis._

class DfaSpec extends FlatSpec with Matchers with OptionValues {
  import Regex._

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')

  behavior of "Dfa.getString"

  it should "return None if the DFA's language is empty 1" in {
    val δ = Map(∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, ∅, Set.empty)
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 2" in {
    val δ = Map(∅ → Seq(!CharSet() → ∅))
    //set of final states is non-empty, but final state is unreachable
    val dfa = Dfa(δ, ∅, Set(b))
    dfa.getString shouldEqual None
  }

  it should "return None if the Dfa's language is empty 3" in {
    val dfa = analyze(!(α.*))
    dfa.getString shouldEqual None
  }

  it should "return None if the Dfa's language is empty 4" in {
    val dfa = analyze(Intersect(b,c))
    dfa.getString shouldEqual None
  }

  // more tests...

  it should "return a string from a DFA with looping paths" in {
    val δ: Transitions[Regex] = Map((b~c).* ~ d -> Seq(CharSet('b') -> c,
                                            CharSet('d') -> ε,
                                            !(CharSet('b') ++ CharSet('d')) -> ∅),
                                    c -> Seq(CharSet('c') -> (b~c).* ~ d,
                                            !CharSet('c') -> ∅),
                                    ε -> Seq(!CharSet() -> ∅),
                                    ∅ -> Seq(!CharSet() -> ∅))
    val dfa = Dfa(δ, (b~c).* ~ d, Set[Regex](ε))
    val s = dfa.getString.value
    dfa matches s shouldEqual true

  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 1" in {
    val δ: Transitions[Regex] = Map(ε → Seq(!CharSet() → ∅), ∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, ε, Set[Regex](ε))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }


  it should "return a string that the DFA recognizes if the DFA's language is not empty 2" in {
    val δ: Transitions[Regex] = Map(Concatenate(b,c) → Seq(CharSet('b') -> c,
                                                           !CharSet('b') -> ∅),
                                    c -> Seq(CharSet('c') -> ε,
                                            !CharSet('c') -> ∅),
                                    ε -> Seq(!CharSet() -> ∅),
                                    ∅ -> Seq(!CharSet() -> ∅))
    val dfa = Dfa(δ, Concatenate(b,c), Set[Regex](ε))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }


  it should "return a string recognized by aabbcc" in {
    val dfa = analyze("aabbcc".concatenate)
    val s = dfa.getString.value
    s shouldEqual "aabbcc"
    dfa matches s shouldEqual true
  }

  it should "return a string recognized by the complex regex 1" in {
    val re = !α
    val dfa = analyze(re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string recognized by the complex regex 2" in {
    val re = b.+
    val dfa = analyze(re)
    val s = dfa.getString.value
    s shouldEqual "b"
    dfa matches s shouldEqual true
  }

  it should "return a string recognized by an evil regex 1" in {
    val evil_re = Union(b, b~b).+
    val dfa = analyze(evil_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string recognized by an evil regex 2" in {
    val evil_re = KleeneStar(α.+)
    val dfa = analyze(evil_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  // more tests...
}
