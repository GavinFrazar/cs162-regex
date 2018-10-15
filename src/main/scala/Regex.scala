// This file provides the abstract syntax for regular expressions.

package edu.ucsb.cs.cs162.regex

import edu.ucsb.cs.cs162.range_set._

// Abstract syntax tree data structure for regular expressions.
sealed abstract class Regex {
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  override def toString = this match {
    case `∅` => "∅"
    case `ε` => "ε"
    case `α` => "α"
    case Chars(charset) => charset.toString
    case Concatenate(re1, re2) => s"(${re1.toString} ~ ${re2.toString})"
    case Union(re1, re2) => s"(${re1.toString} | ${re2.toString})"
    case KleeneStar(re1) => s"(${re1.toString})*"
    case Complement(re1) => s"!(${re1.toString})"
    case Intersect(re1, re2) => s"(${re1.toString} & ${re2.toString})"
  }

  // Output the expression's structure as a tree.
  def prettyPrint: String = asciiTree(this, prefix = "")

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Translate the abstract syntax tree into an ASCII tree structure.
  private def asciiTree(re: Regex, prefix: String): String = {
    // The pieces that the ASCII tree structure are composed of.
    val (tee, bar, cap, end) = ("├─ ", "│  ", "└─ ", "   ")

    re match {
      case `∅` => "∅\n"
      case `ε` => "ε\n"
      case charset: Chars => charset.toString + "\n"
      case Concatenate(re1, re2) => {
        "Concatenate\n" + prefix + tee + asciiTree(re1, prefix + bar) +
        prefix + cap + asciiTree(re2, prefix + end)
      }
      case u @ Union(re1, re2) => {
        s"Union\n" +
        prefix + tee + asciiTree(re1, prefix + bar) +
        prefix + cap + asciiTree(re2, prefix + end)
      }
      case k @ KleeneStar(re1) => {
        s"KleeneStar\n" +
        prefix + cap + asciiTree(re1, prefix + end)
      }
      case Complement(re1) => {
        "Complement\n" + prefix + cap + asciiTree(re1, prefix + end)
      }
      case Intersect(re1, re2) => {
        "Intersect\n" + prefix + tee + asciiTree(re1, prefix + bar) +
        prefix + cap + asciiTree(re2, prefix + end)
      }
    }
  }
}

// A mostly-standard regular expression definition, including complement and
// intersection. Note that we use sets of characters rather than single
// characters and that the empty language is represented as an empty set of
// characters.
case object EmptyString extends Regex
case class Chars(chars: CharSet) extends Regex
case class Concatenate(a: Regex, b: Regex) extends Regex
case class Union(a: Regex, b: Regex) extends Regex
case class KleeneStar(a: Regex) extends Regex
case class Complement(a: Regex) extends Regex
case class Intersect(a: Regex, b: Regex) extends Regex

// Convenient constructors for character set expressions.
object Chars {
  // Use as, e.g., Chars('a', 'c', 'd').
  def apply(chars: Char*): Chars = Chars(CharSet(chars: _*))

  // Use as, e.g., Chars('a' -> 'd', 'f' -> 'i').
  def apply(range: (Char, Char), ranges: (Char, Char)*): Chars =
    Chars(CharSet(range, ranges: _*))
}

object Regex {
  // Convenient notation shortcuts for the empty language, empty string, and
  // "any character".
  final val ∅ = Chars()
  final val ε = EmptyString
  final val α = Chars(Char.MinValue -> Char.MaxValue)
}



