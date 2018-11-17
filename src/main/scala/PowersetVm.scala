package edu.ucsb.cs.cs162.regex.vm

import edu.ucsb.cs.cs162.regex.parse_tree._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

// A virtual machine that uses Thompson's powerset strategy to implement a
// non-backtracking algorithm for regular expression matching.
class PowersetVm(program: Program) extends VirtualMachine(program) {
  override def eval(str: String): Option[ParseTree] = {
    // Algorithm:
    // 1. compute initial set of threads (the ε-closure of the nfa start state)
    // 2. if the input string is empty go to step 7
    // 3. run the threads until they reach a match or accept instruction
    // 4. compact them to enforce at most one thread per program counter
    // 5. execute the surviving threads one step (i.e., the match or accept instruction)
    // 6. go to step 2
    // 7. compact the final set of threads
    // 8. if there is a surviving thread at an accept instruction, then that
    //    thread's 'parse' contains the final answer; otherwise there is no answer

    // Execute all given threads until they reach either a MatchSet or an Accept
    // instruction; returns the resulting set of Threads.
//    @annotation.tailrec
    def runUntilMatchOrAccept(thread: Thread, todo: Set[Thread],
                              result: Set[Thread]): Set[Thread] = {

      program(thread.pc) match {
        case `Accept` => {
          if (todo.isEmpty)
            result + thread
          else
            runUntilMatchOrAccept(todo.head, todo.tail, result + thread)
        }

        case `Reject` => {
          if (todo.isEmpty)
            result
          else
            runUntilMatchOrAccept(todo.head, todo.tail, result)
        }

        case `CheckProgress` => {
          if (thread.progress.contains(thread.pc)){
            if (todo.isEmpty)
              result
            else
              runUntilMatchOrAccept(todo.head, todo.tail, result)
          }else{
            runUntilMatchOrAccept(
              thread.advance(1, addProgress=true),
              todo,
              result)
          }
        }

        case MatchSet(chars: CharSet) => {
          if (todo.isEmpty)
            result + thread
          else
            runUntilMatchOrAccept(todo.head, todo.tail, result + thread)
        }

        case Jump(offset: Int) => {
          runUntilMatchOrAccept(thread.advance(1), todo, result)
        }

        case Fork(off1, off2) => {
          val newThread = thread.copy(pc = thread.pc + off2)
          runUntilMatchOrAccept(thread.advance(off1), todo + newThread, result) 
        }

        case instruction: Instruction => {
          runUntilMatchOrAccept(
            thread.advance(1, Some(instruction)),
            todo,
            result
          )
        }
      }
    }

    // Remove any threads s.t. there exists another thread at the same program
    // point with a smaller Priority.
    def compact(threads: Set[Thread]): Set[Thread] = ???

    // Return the result of matching the current string position on all the
    // given threads.
    val matchStringPosition: (Set[Thread], Char) => Set[Thread] = ???

    val initialThreads =
      runUntilMatchOrAccept(Thread(0,Set(),"",Seq()), Set(), Set())
  }

  // A thread of execution for the VM, where 'pc' is the program counter,
  // 'progress' is the set of encountered CheckProgress instructions, 'priority'
  // is the thread priority (lower is better), 'parse' is the current parsing
  // stack. We don't need a separate string position per thread because all
  // executing threads will, by construction, always be at the same string
  // position.
  private case class Thread(pc: Int, progress: Set[Int], priority: String,
    parse: Seq[ParseTree])

  private implicit class ThreadAdvance(thread: Thread) {
    def advance(increment: Int,
                instruction: Option[Instruction] = None,
                addProgress: Boolean = false): Thread = {
      val progress = addProgress match {
        case false => thread.progress
        case true => thread.progress + thread.pc
      }

      val parse = instruction match {
        case None => thread.parse
        case Some(pN) => pN match {
          case `PushEmpty` => EmptyLeaf +: thread.parse
          case `PushConcat` => {
            val right = thread.parse.head
            val left = thread.parse.tail.head
            val rest = thread.parse.tail.tail
            ConcatNode(left,right) +: rest
          }
          case `PushLeft` => LeftNode(thread.parse.head) +: thread.parse.tail
          case `PushRight` => RightNode(thread.parse.head) +: thread.parse.tail
          case `InitStar` => StarNode(Seq()) +: thread.parse
          case `PushStar` => {
            val body = thread.parse.head
            val star = thread.parse.tail.head
            val rest = thread.parse.tail.tail
            star match {
              case StarNode(seq) => StarNode(body +: seq) +: rest
              case _ =>
                assert(false, "should be unreachable")
                thread.parse
            }
          }
          case PushCapture(name) =>
            CaptureNode(name, thread.parse.head) +: thread.parse.tail
        }
      }

      Thread(thread.pc + increment,
             progress,
             thread.priority,
             parse)
    }
  }
}
