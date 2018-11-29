// This file uses the 'pimp my library' pattern to add builder methods and regex
// operations to Regex.

package edu.ucsb.cs.cs162.regex

import edu.ucsb.cs.cs162.regex.derivative._
import Regex._

object `package` {
  implicit class RegexToDfaAnalysis(re: Regex){
    def getMatch: Option[String] = {
      DerivativeAnalysis.analyze(re).getString
    }
  }

  // Convenient methods to build regular expressions.
  implicit class RegexBuilder(val re: Regex) extends AnyVal {
    //----------------------------------------------------------------------------
    // Public API.
    //----------------------------------------------------------------------------

    // Concatenate 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def ~(other: Regex): Regex = (re, other) match {
      case (`∅`, r) => `∅`
      case (r, `∅`) => `∅`
      case (`ε`, r) => r 
      case (r, `ε`) => r
      // This case should come after all other cases that handle concatenation
      // simplification. It handles the case where the concatenation is not
      // right-associative, and transforms it into right-associative form. There
      // should be a default case after this one that handles anything that this
      // case and the prior simplification cases don't handle.
      case (_: Concatenate, _) => {
        // Replace the right-most concatenation in 're' with the concatenation
        // to 'other'.
        def replaceRight(re: Regex) : Regex = re match {
          case Concatenate(re1, re2) => Concatenate(re1, replaceRight(re2))
          case _ => Concatenate(re, other)
        }
        replaceRight(re)
      }
      case (r1, r2) => Concatenate(r1, r2)
    }

    // Union 're' with 'other', simplifying if possible (assumes that 're' and
    // 'other' have already been simplified).
    def |(other: Regex): Regex = (re, other) match {
      case (r, `∅`) => r
      case (`∅`, r) => r
      case (Chars(a), Chars(b)) => Chars(a ++ b) 
      case (KleeneStar(r), `ε`) => KleeneStar(r) 
      case (`ε`, KleeneStar(r)) => KleeneStar(r)
      case (KleeneStar(`α`), r) => KleeneStar(`α`)
      case (r, KleeneStar(`α`)) => KleeneStar(`α`)
      case (r1, r2) if r1 == r2 => r1
      // This case should come after all other cases that handle union
      // simplification. It ensures that unions are right-associative and the
      // operands are ordered correctly.
      case _ => {
        // Collect together all immediate non-Union sub-expressions.
        def collect(re: Regex): Set[Regex] = re match {
          case Union(re1, re2) => collect(re1) ++ collect(re2)
          case _ => Set(re)
        }
        val subexpressions = collect(re) ++ collect(other)
        associate(subexpressions.toSeq, (re1, re2) => Union(re1, re2))
      }
    }

    // Apply the Kleene star to 're', simplifying if possible (assumes that 're'
    // has already been simplified).
    def * : Regex = re match {
      case `∅` => `ε`
      case `ε` => `ε`
      case KleeneStar(r) => KleeneStar(r)
      case r => KleeneStar(r)
    }

    // Complement 're', simplifying if possible (assumes that 're' has already
    // been simplified).
    def unary_! : Regex = re match {
      case Complement(r) => r
      case `∅` => `α`.*
      case `ε` => `α` ~ `α`.*
      case r => Complement(r)
    }

    // Intersect 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def &(other: Regex): Regex = (re, other) match {
      case (`∅`, _) => `∅`
      case (_, `∅`) => `∅`
      case (Chars(a), Chars(b)) => Chars(a & b)
      case (KleeneStar(`α`), r) => r
      case (r, KleeneStar(`α`)) => r
      case (r1, r2) if r1 == r2 => r1
      // This case should come after all other cases that handle intersection
      // simplification. It ensures that intersections are right-associative and
      // the operands are ordered correctly.
      case _ => {
        // Collect together all immediate non-Intersect sub-expressions.
        def collect(re: Regex): Set[Regex] = re match {
          case Intersect(re1, re2) => collect(re1) ++ collect(re2)
          case _ => Set(re)
        }
        val subexpressions = collect(re) ++ collect(other)
        associate(subexpressions.toSeq, (re1, re2) => Intersect(re1, re2))
      }
    }

    // Shorthand for 1 or more repetitions of re regex.
    def + : Regex = re ~ re.*

    // Shorthand for 0 or 1 instances of re regex.
    def ? : Regex = `ε` | re

    // Shorthand for exactly 'num' repetitions of re regex.
    def ^(num: Int): Regex = {
      require(num >= 0)

      @annotation.tailrec
      def iter(acc: Regex, steps: Int): Regex = steps match {
        case 0 => acc
        case _ => iter(acc ~ re, steps - 1)
      }
      iter(`ε`, num)
    }

    // Shorthand for at least 'min' repetitions of re regex.
    def >=(min: Int): Regex = (re^min) ~ re.*

    // Shorthand for at most 'max' repetitions of re regex.
    def <=(max: Int): Regex = {
      require(max >= 0)

      @annotation.tailrec
      def iter(acc: Regex, steps: Int): Regex = {
        if (steps > max)
          acc
        else
          iter(acc | re^steps, steps + 1)
      }
      iter(`ε`, 1)
    }

    // Shorthand for at least 'min' but at most 'max' repetitions of re regex.
    def <>(min: Int, max: Int): Regex = (re >= min) & (re <= max)

    // Place the regex inside a capture group with the given name.
    def capture(name: String): Regex =
      Capture(name, re)

    //----------------------------------------------------------------------------
    // Private details.
    //----------------------------------------------------------------------------

    // Sort the subterms of a Regex in lexicographic order.
    private def associate(res: Seq[Regex], join: (Regex, Regex) => Regex): Regex =
      res.sortWith((re1, re2) => re2 lessThanEq re1).reduceLeft(
        (acc, re) => join(re, acc))
  }

    // Add convenient methods to String for building simple regular expressions.
    implicit class StringToRegex(val str: String) extends AnyVal {
      // Builds the concatenation of each character in 'str' in sequence. Example:
      // "abc".concatenate == Chars('a') ~ Chars('b') ~ Chars('c').
      def concatenate: Regex =
        str.foldLeft(ε: Regex)((acc, char) => acc ~ Chars(char))

      // Builds a charset containing each character in 'str'. Example:
      // "abc".charset == Chars('a', 'b', 'c').
      def charset: Regex =
        if (str.isEmpty) ε else Chars(str.toSeq: _*)
    }
  
  // Operations on regular expressions.
  implicit class RegexOps(val re: Regex) extends AnyVal {
    // Returns ε if 're' is nullable, otherwise returns ∅.
    def nullable: Regex = re match {
      case `ε` | _: KleeneStar => ε
      case `∅` | _: Chars => ∅
      case Concatenate(re1, re2) => re1.nullable ~ re2.nullable
      case Union(re1, re2) => re1.nullable | re2.nullable
      case Complement(re1) => if (re1.nullable == ε) ∅ else ε
      case Intersect(re1, re2) => re1.nullable & re2.nullable
      case Capture(_, child) => child.nullable
    }

    // Returns true iff the language recognized by 're' is empty.
    def empty: Boolean =
      DerivativeAnalysis.analyze(re).fin.isEmpty

    // Returns true iff the language of 'other' is contained in the language of
    // 're'.
    def contains(other: Regex): Boolean =
      (other & !re).empty

    // Returns true iff the language of 're' is the same as the language of
    // 'other'.
    def equivalent(other: Regex): Boolean =
      re.contains(other) && other.contains(re)

    // Returns true if 're' <= 'other' according to a lexicographic ordering
    // of the regex ASTs.
    def lessThanEq(other: Regex): Boolean = (re, other) match {
      case (`∅`, _) => true
      case (_, `∅`) => false
      case (`ε`, _) => true
      case (_, `ε`) => false
      case (Chars(c1), Chars(c2)) => c1.toString <= c2.toString
      case (_: Chars, _) => true
      case (_, _: Chars) => false
      case (Concatenate(re1, re2), Concatenate(reA, reB)) => {
        if (re1 == reA) re2 lessThanEq reB
        else re1 lessThanEq reA
      }
      case (_: Concatenate, _) => true
      case (_, _: Concatenate) => false
      case (Union(re1, re2), Union(reA, reB)) => {
        if (re1 == reA) re2 lessThanEq reB
        else re1 lessThanEq reA
      }
      case (_: Union, _) => true
      case (_, _: Union) => false
      case (KleeneStar(re1), KleeneStar(reA)) => re1 lessThanEq reA
      case (_: KleeneStar, _) => true
      case (_, _: KleeneStar) => false
      case (Capture(name1, re1), Capture(name2, re2)) => {
        (name1 <= name2) && (re1 lessThanEq re2)
      }
      case (_: Capture, _) => true
      case (_, _: Capture) => false
      case (Complement(re1), Complement(reA)) => re1 lessThanEq reA
      case (_: Complement, _) => true
      case (_, _: Complement) => false
      case (Intersect(re1, re2), Intersect(reA, reB)) => {
        if (re1 == reA) re2 lessThanEq reB
        else re1 lessThanEq reA
      }
      case (_: Intersect, _) => true
      case (_, _: Intersect) => false
    }

    // Returns the expression that recognizes the reverse language of re.
    def reverse: Regex = re match {
      case `ε` | `∅` | _: Chars => re
      case Concatenate(re1, re2) => re2.reverse ~ re1.reverse
      case Union(re1, re2) => re1.reverse | re2.reverse
      case KleeneStar(re1) => re1.reverse.*
      case Complement(re1) => !re1.reverse
      case Intersect(re1, re2) => re1.reverse & re2.reverse
      case cap @ Capture(name, re1) => re1.reverse.capture(name)
    }

    // Returns the set of words that are strict suffixes of words in 're'.
    def strictSuffix: Regex = {
      val star = re.isInstanceOf[KleeneStar]
      DerivativeAnalysis.derivativeClosure(re).foldLeft(∅ : Regex)(
        (acc, deriv) => if (re == deriv && !star) acc else acc | deriv
      )
    }

    // Returns the set of words that are strict prefixes of words in 're'.
    def strictPrefix: Regex =
      re.reverse.strictSuffix.reverse

    // Returns the set of words that are suffixes of words in 're'; includes all
    // words in 're'.
    def suffix: Regex =
      re.strictSuffix | re

    // Returns the set of words that are prefixes of words in 're'; includes all
    // words in 're'.
    def prefix: Regex =
      re.strictPrefix | re

    // Returns the set of words that are substrings of words in 're'.
    def substrings: Regex =
      re.suffix.prefix

    // Returns the overlap language of 're' and 'other', i.e. re.overlap(other)
    // = { w1~w2~w3 | w1, w1~w2 ∈ re; w2~w3, w3 ∈ other; w2 ≠ ε }
    def overlap(other: Regex): Regex = {
      val containsW1 = re.strictPrefix & re
      val containsW2 = re.suffix & other.prefix & !ε
      val containsW3 = other.strictSuffix & other

      val w1w2 = (containsW1 ~ containsW2) & re
      val w2 = w1w2.suffix & containsW2
      val w2w3 = (w2 ~ containsW3) & other

      (w1w2 ~ containsW3) & (containsW1 ~ w2w3)
    }

    // Determines if 're' is unambiguous or not. If so, returns None. Otherwise,
    // returns the ambiguous sub-expression (the first one for which ambiguity
    // is detected, if there is more than one) and a string that exposes the
    // ambiguity of that sub-expression.
    def unambiguous: Option[(Regex, String)] = {
      def helper(re: Regex): Option[(Regex, String)] = re match{
        case `∅` => None
        case `ε` => None
        case Chars(cs) => None
        case Union(r1, r2) => {
          val curr = (r1 & r2)
          val left = helper(r1)
          val right = helper(r1)
          return (curr.empty, left, right) match {
            case (true, None, None) => None
            case (false, _, _) => Some(re -> curr.getMatch.get)
            case (_, Some((_, str)), _) => Some(r1 -> str)
            case (_, _, Some((_, str))) => Some(r2 -> str)
          }
        }
        case Concatenate(r1, r2) => {
          val curr = (r1 overlap r2)
          val left = helper(r1)
          val right = helper(r2)
          return (curr.empty, left, right) match {
            case (true, None, None) => None
            case (false, _, _) => Some(re -> curr.getMatch.get)
            case (_, Some((_,str)), _) => Some(r1 -> str)
            case (_, _, Some((_,str))) => Some(r2 -> str)
          }
        }
        case KleeneStar(r) => {
          val curr = (r overlap r.*)
          val other = helper(r)
          return (curr.empty && r.nullable == ∅, other) match {
            case (true, None) => None
            case (false, _) => Some(re -> curr.getMatch.get)
            case (_, Some((_, str))) => Some(r -> str)
          }
        }
        case Capture(str, r) => {
          return helper(r) match {
            case None => None
            case Some((_,str)) => Some(r -> str)
          }
        }
        case _ => {
          assert(false, "We dont handle complement/intersection.")
          Some(re -> "stub")
        }
      }
      helper(re)
    }
  }
}
