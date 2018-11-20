// This is the compiler that translates regex abstract syntax trees into regular
// expression matching virtual machine programs.

package edu.ucsb.cs.cs162.regex.vm.compiler

import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._
import Regex._

object Compiler {
  // Return a virtual machine program that implements the given regex.
  def compile(re: Regex): Program = {
    def recursive_compile(re: Regex): Program = re match {
      case `∅` => IndexedSeq(Reject)
      case `ε` => IndexedSeq(PushEmpty)
      case Chars(cs) => IndexedSeq(MatchSet(cs), PushChar)
      case Concatenate(r,s) => {
        (recursive_compile(r) ++: recursive_compile(s)) :+ PushConcat
      }
      case Union(l,r) => {
        val left = recursive_compile(l) :+ PushLeft
        val right = recursive_compile(r) :+ PushRight
        ((Fork(1, left.length+2) +: left) ++:
           (Jump(right.length + 1) +: right))
      }
      case KleeneStar(re) => {
        val inside = recursive_compile(re)
        val outside = re.nullable match {
          case `ε` => {
            IndexedSeq(CheckProgress, Fork(1, inside.length+3)) ++: inside ++:
              IndexedSeq(PushStar, Jump(-1*(inside.length+3)))
          }
          case `∅` => {
            IndexedSeq(Fork(1, inside.length+3)) ++: inside ++:
            IndexedSeq(PushStar,Jump(-1*(inside.length+2)))
          }
          case _ => {
            assert(false, "nullable should always return ε or ∅")
            val stub = IndexedSeq()
            stub
          }
        }
        InitStar +: outside
      }
      case Capture(name, re) => recursive_compile(re) :+ PushCapture(name)
      case _ => IndexedSeq(Reject)
    }
    recursive_compile(re) :+ Accept
  }
}
