// Provides a derivative-based static analysis of regular expressions that
// yields a DFA describing the language recognized by an expression.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.dfa._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

object DerivativeAnalysis {
  import Derive._
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Statically analyzes 're' using derivatives in order to compute the DFA of
  // the language recognized by 're'. The resulting DFA has an explicit error
  // state and is approximately minimal.
  def analyze(re: Regex): Dfa[Regex] = {
    val (states, transitions) = computeDfa(Set(re), Set[Regex](), Map())
    val fin = (Set[Regex]() /: states) {
      (acc_set, state) => {
        if (state.nullable == ε)
          acc_set + state
        else
          acc_set
      }
    }
    Dfa(transitions, re, fin)
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Compute the transitions and set of reachable states (i.e., Regexes) for all
  // Regexes in 'todo'.
  @annotation.tailrec
  private def computeDfa(todo: Set[Regex],
                         visitedStates: Set[Regex],
                         transitions: Transitions[Regex]
  ): (Set[Regex], Transitions[Regex]) = {
    if (todo.isEmpty)
      (visitedStates, transitions)
    else {
      if (visitedStates.contains(todo.head))
        computeDfa(todo.tail, visitedStates, transitions)
      else{
        val (new_states, new_transitions) = computeNext(todo.head)
        computeDfa(todo.tail ++ new_states,
                   visitedStates + todo.head,
                   transitions ++ new_transitions
        )
      }
    }
  }

  implicit class PairWiseUnion(self: Set[CharSet]) {
    def ^(other: Set[CharSet]): Set[CharSet] = {
      self flatMap {
        self_charset => (Set[CharSet]() /: other) {
          (acc_set, other_charset) => acc_set + (self_charset & other_charset)
        }
      }
    }
  }

  def C(r: Regex): Set[CharSet] = {
    val Σ = α.chars
    r match {
      case `∅` => Set(Σ)
      case `ε` => Set(Σ)
      case Chars(s) => Set(s, Σ & (!s))
      case KleeneStar(r) => C(r)
      case Complement(r) => C(r)
      case Union(r, s) => C(r) ^ C(s)
      case Intersect(r, s) => C(r) ^ C(s)
      case Concatenate(r, s) => (!r).nullable match {
        case `ε` => C(r)
        case _ => C(r) ^ C(s)
      }
    }
  }

  // Compute the transitions and destination states from the given regex.
  private def computeNext(state: Regex): (Set[Regex], Transitions[Regex]) = {
    val dvm = DerivativeMachine(state)
    val partitions = C(state)
    val transition_states = (Seq[(CharSet, Regex)]() /: partitions){
      (acc_states, partition) => partition.minElement match {
        case Some(char) => acc_states :+ (partition, dvm.derive(char))
        case None => acc_states
      }
    }
    val transitions = Map(state -> transition_states)
    val dst_states = transition_states.map(_._2).toSet
    (dst_states, transitions)
  }
}
