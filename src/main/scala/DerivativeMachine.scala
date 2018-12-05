// A virtual machine implementation of derivative-based matching.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.regex._

object `package` {
  // Programs for the DerivativeMachine.
  type Program = Seq[Instruction]

  // Pretty-print derivative virtual machine programs.
  def programToString(prog: Program): String = {
    val strs = for (inst <- prog) yield inst match {
      case `PushDerive` => "derive"
      case `PushConcatenate` => "concatenate"
      case `PushUnion` => "union"
      case `PushComplement` => "complement"
      case `PushIntersect` => "intersect"
      case `PushNullable` => "nullable"
      case PushRe(re) => "push " + re.toString
    }
  
    strs.mkString("\n")
  }
}

// Instructions for the virtual machine.
//
// - Derive: pop the top of the operand stack, compute its derivative w.r.t. the
//   machine's given char, then push the result back on the operand stack.
// - PushConcatentate: pop the top two elements of the operand stack and push
//   their concatenation back on.
// - PushUnion: pop the top two elements of the operand stack and push their
//   union back on.
// - PushComplement: pop the top of the operand stack, take its complement, and
//   push the result back on.
// - PushIntersect: pop the top two elements of the operand stack and push
//   their intersection back on.
// - PushNullable: pop the top of the operand stack, compute its nullability,
//   and push the result back on the operand stack.
// - PushRe(re): push re onto the top of the operand stack.
sealed abstract class Instruction
case object PushDerive extends Instruction
case object PushConcatenate extends Instruction
case object PushUnion extends Instruction
case object PushComplement extends Instruction
case object PushIntersect extends Instruction
case object PushNullable extends Instruction
case class PushRe(re: Regex) extends Instruction

class DerivativeMachine(re: Regex) {
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff 'str' is recognized by 're'.
  def eval(str: String): Boolean = {
    (re /: str)(
      (currentRe,char) => run(Seq(currentRe), Seq(PushDerive), char)
    ).nullable == ε
  }

  // Returns the derivative of 're' w.r.t. 'char'.
  def derive(char: Char): Regex = run(Seq(re), Seq(PushDerive), char)

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  @annotation.tailrec
  private def run(operands: Seq[Regex], program: Program, char: Char): Regex = {
    if (program.isEmpty) {
      assert(operands.size == 1)
      operands.head
    }
    else {
      program.head match {
        case PushDerive => operands.head match {
          case `∅` | `ε` => run(∅ +: operands.drop(1), program.drop(1), char) 
          case Chars(chars) => {
            val r = ( 
              if (chars.contains(char))
                ε
              else
                ∅
            )
            run(r +: operands.drop(1), program.drop(1), char)
          }
          case Concatenate(r1, r2) => {
            run(operands.drop(1),
                Seq(PushRe(r2),
                    PushDerive,
                    PushRe(r1),
                    PushNullable,
                    PushConcatenate,
                    PushRe(r2),
                    PushRe(r1),
                    PushDerive,
                    PushConcatenate,
                    PushUnion) ++
                  program.drop(1),
                char
            )
          }
          case Union(r1, r2) => {
            run(operands.drop(1),
                Seq(PushRe(r2),
                    PushDerive,
                    PushRe(r1),
                    PushDerive,
                    PushUnion) ++
                  program.drop(1),
                char
            )
          }
          case rek @ KleeneStar(r) => {
            run(operands.drop(1),
                Seq(PushRe(rek),
                    PushRe(r),
                    PushDerive,
                    PushConcatenate) ++
                  program.drop(1),
                char
            )
          }
          case Complement(r) => {
            run(operands.drop(1),
                Seq(PushRe(r),
                    PushDerive,
                    PushComplement) ++
                  program.drop(1),
                char
            )
          }
          case Intersect(r1, r2) => {
            run(operands.drop(1),
                Seq(PushRe(r2),
                    PushDerive,
                    PushRe(r1),
                    PushDerive,
                    PushIntersect) ++
                  program.drop(1),
                char
            )
          }
        }
        case PushConcatenate => {
          val (r1, r2) = (operands(0), operands(1))
          run((r1 ~ r2) +: operands.drop(2), program.drop(1), char)
        }
        case PushUnion => {
          val (r1, r2) = (operands(0), operands(1))
          run((r1 | r2) +: operands.drop(2), program.drop(1), char)
        }
        case PushComplement => {
          val r = operands(0)
          run((!r) +: operands.drop(1), program.drop(1), char)
        }
        case PushIntersect => {
          val (r1, r2) = (operands(0), operands(1))
          run((r1 & r2) +: operands.drop(2), program.drop(1), char)
        }
        case PushNullable => {
          val r = operands(0)
          run(r.nullable +: operands.drop(1), program.drop(1), char)
        }
        case PushRe(r) => run(r +: operands, program.drop(1), char)
      }
    }
  }
}

object DerivativeMachine {
  def apply(re: Regex) = new DerivativeMachine(re)
}
