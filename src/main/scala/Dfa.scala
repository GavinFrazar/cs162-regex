// Provides the Dfa class for deterministic finite automata.

package edu.ucsb.cs.cs162.dfa

import edu.ucsb.cs.cs162.range_set._

object `package` {
  type Transitions[State] = Map[State, Seq[(CharSet, State)]]
}

// The DFA. 'delta' is the state transition map; 'init' is the initial state;
// 'fin' is the set of final states. The DFA is assumed to have an explicit
// error state and the transitions are assumed to cover the complete range of
// possible characters.
case class Dfa[State](delta: Transitions[State], init: State, fin: Set[State]) {
  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff the given string is recognized by the DFA.
  def matches(str: String): Boolean =
    fin.contains(trace(init, str))

  // Returns a string that causes an arbitrary but non-looping path from the
  // init state to a final state, if such a path exists.
  def getString(): Option[String] = {
    def helper(current_state: State,
               visited: Set[State],
               acc: String
    ): Option[String] = {
      if (fin.contains(current_state))
        Some(acc + current_state.toString)
      else{
        delta(current_state).find(x => visited.contains(x._2) == false) match {
          case Some((_, state)) => helper(state,
                                         visited + state,
                                         acc + current_state.toString)
          case None => None
        }
      }
    }
    helper(init, Set(), "")
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Compute the state reached by tracing the given string through the DFA.
  @annotation.tailrec
  private def trace(state: State, str: String): State =
    if (str.isEmpty) state
    else delta(state).find(_._1.contains(str.head)) match {
      case Some((_, next)) => trace(next, str.tail)
      case None => {
        assert(false, "should be unreachable")
        state
      }
    }
}
