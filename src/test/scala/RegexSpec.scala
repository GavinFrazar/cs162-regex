package edu.ucsb.cs.cs162.regex

import org.scalatest._
import edu.ucsb.cs.cs162.regex.derivative._

class RegexSpec extends FlatSpec with Matchers with OptionValues {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val charA = Chars('a')
  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val e = Chars('e')
  val f = Chars('f')

  val r = Chars('a') | Chars('b').+
  val r1 = Chars('x', 'y').* ~ r
  val r2 = Chars('y', 'x').+ ~ r
  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "a regex"

  it should "be buildable using `~`" in {
    (r1 ~ r2) should equal (Chars('x', 'y').* ~ r ~ Chars('y', 'x').+ ~ r)
    // simplifications
    (r ~ ∅) should equal(∅)
    (∅ ~ r) should equal(∅)
    (r ~ ε) should equal(r)
    (ε ~ r) should equal(r)
  }


  it should "be buildable using `|`" in {
    (r1 | r2) should equal(Union(r2, r1)) // also testing normalization due to lengths of r1 and r2
    // simplifications
    (r | ∅) should equal(r)
    (∅ | r) should equal(r)
    (Chars('a' -> 'c') | Chars('c' -> 'f')) should equal(Chars('a'->'f'))
    (r.* |   ε) should equal(r.*)
    (ε   | r.*) should equal(r.*)
    (α.* |   r) should equal(α.*)
    (r |   α.*) should equal(α.*)
    (r | r)     should equal(r)
  }

  it should "be buildable using `*`" in {
    r.* should equal(KleeneStar(r))
    // simplifications
    ∅.* should equal(ε)
    ε.* should equal(ε)
    (r.*).* should equal(r.*)
  }

  it should "be buildable using `!`" in {
    !r should equal(Complement(r))
    // Simplifications
    !(!r) should equal(r)
    !(∅) should equal(α.*)
    !ε should equal(α.+)
  }

  it should "be buildable using `&`" in {
    (r1 & r2) should equal(Intersect(r2, r1)) // also testing normalization due to lengths of r1 and r2
    // Simplifications
    (∅ & r) should equal(∅)
    (r & ∅) should equal(∅)
    (Chars('a'->'d') & Chars('c'->'f')) should equal (Chars('c'->'d'))
    (α.* & r) should equal(r)
    (r & α.*) should equal(r)
    (r & r) should equal(r)
  }

  it should "be buildable using `^`" in {
    (r^5) should equal(r ~ r ~ r ~ r ~ r)
  }

  it should "be buildable using `>=`" in {
    (r >= 3) should equal(r ~ r ~ r ~ r.*)
  }

  it should "be buildable using `<=`" in {
    (r <= 3) should equal(ε | r | (r ~ r) | (r ~ r ~ r))
  }

  it should "be buildable using `<>`" in {
    (r <>(2, 3)) should equal((r ~ r ~ r.*) & (ε | r | (r ~ r) | (r ~ r ~ r)))
  }


  it should "be buildable using convenience methods 1" in {
    (b ~ c) should equal (Concatenate(b, c))
  }

  it should "be buildable using convenience methods 2" in {
    (b | (b ~ c)) should equal (Union(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 3" in {
    b.* should equal (KleeneStar(b))
  }

  it should "be buildable using convenience methods 4" in {
    !b should equal (Complement(b))
  }

  it should "be buildable using convenience methods 5" in {
    (b & (b ~ c)) should equal (Intersect(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 6" in {
    b.+ should equal (Concatenate(b, KleeneStar(b)))
  }

  it should "be buildable using convenience methods 7" in {
    b.? should equal (Union(ε, b))
  }

  it should "be buildable using convenience methods 8" in {
    b^3 should equal (Concatenate(b, Concatenate(b, b)))
  }

  it should "be buildable using convenience methods 9" in {
    (b >= 2) should equal (Concatenate(b, Concatenate(b, KleeneStar(b))))
  }

  it should "be buildable using convenience methods 10" in {
    (b <= 2) should equal (Union(ε, Union(b, Concatenate(b, b))))
  }

  it should "be buildable using convenience methods 11" in {
    (b <> (1, 3)) should equal (Intersect(Concatenate(b, KleeneStar(b)), Union(ε, Union(b, Union(Concatenate(b, b), Concatenate(b, Concatenate(b, b)))))))
  }

  it should "be buildable from strings" in {
    "ab".charset ~ "cd".concatenate should equal (Concatenate(Chars('a', 'b'),
      Concatenate(Chars('c'), Chars('d'))))
  }

  it should "pretty-print correctly" in {
    (b.? | (c >= 1)).prettyPrint should equal ("""Union
                                                 |├─ ε
                                                 |└─ Union
                                                 |   ├─ b
                                                 |   └─ Concatenate
                                                 |      ├─ c
                                                 |      └─ KleeneStar
                                                 |         └─ c
                                                 |""".stripMargin)
  }

  it should "normalize correctly 1" in {
    val re = ((charA ~ b) ~ (c ~ d)) ~ (e ~ f)

    val norm = Concatenate(charA, Concatenate(b, Concatenate(c,
      Concatenate(d, Concatenate(e, f)))))

    re should equal (norm)
  }

  it should "normalize correctly 2" in {
    val re = (((b | ε) & charA) | !charA | charA.*) | ((charA ~ b) |
      charA | ε)

    val norm = Union(ε, Union(charA, Union(Concatenate(charA, b),
      Union(KleeneStar(charA), Union(Complement(charA), Intersect(charA,
        Union(ε, b)))))))

    re should equal (norm)
  }

  behavior of "nullable"

  it should "recognize a nullable regex 1" in {
    (KleeneStar(∅).nullable) should equal (ε)
  }

  it should "recognize a nullable regex 2" in {
    (ε.nullable) should equal (ε)
  }

  it should "recognize a nullable regex 3" in {
    (Union(∅, ε).nullable) should equal (ε)
  }

  it should "recognize a nullable regex 4" in {
    (Complement(∅).nullable) should equal (ε)
  }

  it should "recognize a nullable regex 5" in {
    (Intersect(b.*, ε).nullable) should equal (ε)
  }

  it should "recognize a nullable regex 6" in {
    (KleeneStar(c).nullable) should equal (ε)
  }


  // more tests...

  it should "recognize a non-nullable regex 1" in {
    (b.nullable) should equal (∅)
  }

  it should "recognize a non_nullable regex 2" in {
    (∅.nullable) should equal (∅)
  }

  it should "recognize a non_nullable regex 3" in {
    (Union(∅, b).nullable) should equal (∅)
  }

  it should "recognize a non_nullable regex 4" in {
    (Complement(ε).nullable) should equal (∅)
  }

  it should "recognize a non_nullable regex 5" in {
    (Intersect(c.*, c >= 1).nullable) should equal (∅)
  }

  it should "recognize a non-nullable regex 6" in {
    (Concatenate(∅, ε).nullable) should equal (∅)
  }

  // more tests...

  behavior of "ambiguity type checker"

  it should "find the ambiguous subexpression and a witness string in an ambiguous regex" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = a ~ (b | ε) ~ (b | ε)
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal ((b | ε) ~ (b | ε))
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  // more tests...

  it should "return None if the string is unambiguous" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = a ~ (b | ε)
    r.unambiguous shouldEqual None
  }

  // more tests...
  behavior of "regex normalization"

  it should "recognize equivalent regex 1" in {
    val re1 = charA | (charA ~ b)
    val re2 = (charA ~ b) | charA
    re1 should equal (re2)
  }

  it should "recognize equivalent regex 2" in {
    val re1 = charA ~ b ~ c 
    val re2 = Concatenate(charA, Concatenate(b, c))
    re1 should equal (re2)
  }
  
  it should "recognize equivalent regex 3" in {
    val re1 = (b | c) ~ d ~ (e | f)
    val re2 = Concatenate((b | c) , Concatenate(d, (e | f)))
    re1 should equal (re2)
  }
  

}
