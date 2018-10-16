// This file uses the 'pimp my library' pattern to add builder methods and regex
// operations to Regex.

package edu.ucsb.cs.cs162.regex

object `package` {
  import Regex._

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
      case (r1, r2) => Union(r1,r2)
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
      case (r1, r2) => Intersect(r1, r2)
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
    }
  }
}
