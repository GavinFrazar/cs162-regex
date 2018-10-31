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
    val fin = (Set[Regex]() /: a) {
      (set, state) => {
        if (state.nullable == ε)
          set + state
        else
          set
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
  ) :
      (Set[Regex], Transitions[Regex]) = {
    if (todo.isEmpty)
      (visitedStates, transitions)
    else {
      val (new_states, new_transitions) = computeNext(todo.head)
      computeDfa(todo.tail ++ new_states,
                 visitedStates ++ new_states,
                 transitions ++ new_transitions
      )
    }
  }

  // Compute the transitions and destination states from the given regex.
  private def computeNext(state: Regex): (Set[Regex], Transitions[Regex]) = ???

}
