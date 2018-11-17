// This is the compiler that translates regex abstract syntax trees into regular
// expression matching virtual machine programs.

package edu.ucsb.cs.cs162.regex.vm.compiler

import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._
import Regex._

object Compiler {
  // Return a virtual machine program that implements the given regex.
  def compile(re: Regex): Program = {
    def compile_helper(re: Regex): Program = re match {
      case `∅` => IndexedSeq(Reject)
      case `ε` => IndexedSeq(PushEmpty)
      case Chars(cs) => IndexedSeq(MatchSet(cs), PushChar)
      case Concatenate(r,s) => {
        (compile_helper(r) ++ compile_helper(s)) :+ PushConcat
      }
      case Union(l,r) => {
        val left = compile_helper(l) :+ PushLeft
        val right = compile_helper(r) :+ PushRight
        ((Fork(1, left.length+2) +: left) ++
           (Jump(right.length + 1) +: right))
      }
      case KleeneStar(re) => {
        val stub = re.nullable match {
          case `ε` => {
            val inside = compile_helper(re)
            IndexedSeq(CheckProgress, Fork(1, inside.length+3)) ++ inside ++
              IndexedSeq(PushStar, Jump(-1*(inside.length+3)))
          }
          case `∅` => {
            val inside = compile_helper(re)
            IndexedSeq(Fork(1, inside.length+3)) ++ inside ++
            IndexedSeq(PushStar,Jump(-1*(inside.length+2)))
          }
        }
        InitStar +: stub
      }
      case Capture(_, re) => compile_helper(re)
      case _ => IndexedSeq(Reject)
    }
    compile_helper(re) :+ Accept
  }
}
