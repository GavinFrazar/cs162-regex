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
    @annotation.tailrec
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
          val resetThread = thread.copy(progress=Set[Int]())
          if (todo.isEmpty)
            result + resetThread
          else
            runUntilMatchOrAccept(todo.head, todo.tail, result + resetThread)
        }

        case Jump(offset: Int) => {
          runUntilMatchOrAccept(thread.advance(offset), todo, result)
        }

        case Fork(off1, off2) => {
          val newThread = thread.copy(pc = thread.pc + off2)
          runUntilMatchOrAccept(thread.advance(off1), todo + newThread, result) 
        }

        case `PushEmpty` => {
          val newParse = EmptyLeaf +: thread.parse
          runUntilMatchOrAccept(thread.advance(1, parse=newParse), todo, result)
        }

        case `PushChar` => runUntilMatchOrAccept(thread.advance(1), todo, result)

        case `PushConcat` => {
          val right = thread.parse.head
          val left = thread.parse.tail.head
          val rest = thread.parse.tail.tail
          val newParse = ConcatNode(left,right) +: rest 
          runUntilMatchOrAccept(thread.advance(1, parse=newParse), todo, result)
        }

        case `PushLeft` => {
          val newParse = LeftNode(thread.parse.head) +: thread.parse.tail
          runUntilMatchOrAccept(
            thread.advance(1, parse=newParse, priority=thread.priority + "l"),
            todo,
            result
          )
        }

        case `PushRight` => {
          val newParse = RightNode(thread.parse.head) +: thread.parse.tail
          runUntilMatchOrAccept(
            thread.advance(1, parse=newParse, priority=thread.priority + "r"),
            todo,
            result
          )
        }

        case `InitStar` => {
          val newParse = StarNode(Seq()) +: thread.parse
          runUntilMatchOrAccept(thread.advance(1, parse=newParse), todo, result)
        }

        case `PushStar` => {
          val body = thread.parse.head
          val star = thread.parse.tail.head
          val rest = thread.parse.tail.tail
          star match {
            case StarNode(seq) => {
              val newParse = StarNode(body +: seq) +: rest
              runUntilMatchOrAccept(
                thread.advance(1, parse=newParse),
                todo,
                result
              )
            }
            case _ => {
              assert(false, "should be unreachable")
              runUntilMatchOrAccept(thread, todo, result)
            }
          }
        }

        case PushCapture(name) => {
          val newParse =
            CaptureNode(name, thread.parse.head) +: thread.parse.tail
          runUntilMatchOrAccept(thread.advance(1, parse=newParse), todo, result)
        }
      }
    }

    // Remove any threads s.t. there exists another thread at the same program
    // point with a smaller Priority.
    def compact(threads: Set[Thread]): Set[Thread] = {
      def iter(todo: Set[Thread], result: Set[Set[Thread]]): Set[Set[Thread]]= {
        if (todo.isEmpty)
          result
        else{
          val partition = todo.filter(thread => thread.pc == todo.head.pc)
          iter(todo -- partition, result + partition)
        }
      }
      val partitions = iter(threads, Set())
        (Set[Thread]() /: partitions) {
        (acc, partition) => (acc + partition.minBy(e => e.priority))
      }
    }

    // Return the result of matching the current string position on all the
    // given threads.
    val matchStringPosition: (Set[Thread], Char) => Set[Thread] =
      (threads: Set[Thread], char: Char) => {
        threads.filter(
          thread => program(thread.pc) match {
            case MatchSet(cs) => cs.contains(char)
            case Accept => false
            case _ => {
              assert(false, "Instruction should be either match or accept")
              false
            }
          }
        ).map(thread => {
                val newParse = CharLeaf(char) +: thread.parse
                thread.advance(2, parse=newParse)
              }
        )
      }

    val initialThread = Thread(0, Set(), "", Seq())
    val initialThreads =
      compact(runUntilMatchOrAccept(initialThread, Set(), Set()))
    assert(!initialThreads.isEmpty, "There should be at least one init thread.")

    val survivors = 
      (initialThreads /: str){
        (threads, char) => {
          val matchedThreads = matchStringPosition(threads, char)
          if (matchedThreads.isEmpty)
            matchedThreads
          else
            compact(
              runUntilMatchOrAccept(
                matchedThreads.head,
                matchedThreads.tail,
                Set()
              )
            )
        }
      }
    val finalSurvivors: Set[Thread] = compact(survivors)
    val acceptors: Set[Thread] =
      finalSurvivors.filter(thread => program(thread.pc) == Accept)
    if (acceptors.isEmpty)
      None
    else
      Some(acceptors.head.parse.head)
  }

  // A thread of execution for the VM, where 'pc' is the program counter,
  // 'progress' is the set of encountered CheckProgress instructions, 'priority'
  // is the thread priority (lower is better), 'parse' is the current parsing
  // stack. We don't need a separate string position per thread because all
  // executing threads will, by construction, always be at the same string
  // position.
  private case class Thread(pc: Int, progress: Set[Int], priority: String,
    parse: Seq[ParseTree])

  private implicit class ForkThread(thread: Thread){
    def forkLeft(): Thread = thread.copy(priority = thread.priority + "l")
    def forkRight(): Thread = thread.copy(priority = thread.priority + "r")
  }

  private implicit class ThreadUpdater(thread: Thread) {
    def advance(increment: Int,
                addProgress: Boolean = false,
                priority: String = thread.priority,
                parse: Seq[ParseTree] = thread.parse): Thread = {

      val progress = addProgress match {
        case false => thread.progress
        case true => thread.progress + thread.pc
      }

      Thread(
        thread.pc + increment,
        progress,
        priority,
        parse
      )
    }
  }
}
