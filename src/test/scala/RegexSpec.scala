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
  val complex_unambiguous = (b^6) ~ (c^6) ~ (c | ε)

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
    val r = c ~ (b | ε) ~ (b | ε)
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal ((b | ε) ~ (b | ε))
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }


  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 3" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = Union(a|b, b)
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (r)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }


  it should "return None if the expression is ∅" in {
    ∅.unambiguous shouldEqual None
  }


  it should "return None if the expression is Chars" in {
    b.unambiguous shouldEqual None
    α.unambiguous shouldEqual None
  }


  it should "return None if the expression is ε" in {
    ε.unambiguous shouldEqual None
  }


  it should """find the ambiguous subexpression and a witness string in an
            |ambiguous regex, where the ambiguity is due to intersection
            |of inner expressions of a Union"""
    .stripMargin.replaceAll("\n", " ") in {
      val r = Union(b, b)
      val (ambiguousExpr, witness) = r.unambiguous.value
      ambiguousExpr shouldEqual r
      val dvm = new DerivativeMachine(ambiguousExpr)
      dvm.eval(witness) shouldEqual true
    }


  it should """find the ambiguous subexpression and a witness string in an
               |ambiguous regex, where the ambiguity is found
               |in the left or right inner expression of a union"""
    .stripMargin.replaceAll("\n"," ") in {
      val ambiguity = ((c^2).* ~ (c^4).*)
      val r1 = Union(b, ambiguity)
      val r2 = Union(ambiguity, b)
      val (ambiguousSubexprRight, witnessRight) = r1.unambiguous.value
      val (ambiguousSubexprLeft, witnessLeft) = r2.unambiguous.value
      ambiguousSubexprRight shouldEqual ambiguity
      ambiguousSubexprLeft shouldEqual ambiguity
      val dvm = DerivativeMachine(ambiguity)
      dvm.eval(witnessRight) shouldEqual true
      dvm.eval(witnessLeft) shouldEqual true
  }


  it should """return None if the regex is an unambiguous Union""" in {
    val r1 = Union(complex_unambiguous, d)
    val r2 = Union(d, complex_unambiguous)
    r1.unambiguous shouldEqual None
    r2.unambiguous shouldEqual None
  }


  it should """find the ambiguous subexpression and a witness string in an
            |ambiguous regex, where the ambiguity is due to overlap
            |of children of a Concatenation"""
    .stripMargin.replaceAll("\n", " ") in {
      val r = Concatenate(b.?, b.?)
      val (ambiguousExpr, witness) = r.unambiguous.value
      ambiguousExpr shouldEqual r
      val dvm = new DerivativeMachine(ambiguousExpr)
      dvm.eval(witness) shouldEqual true
    }


  it should """find the ambiguous subexpression and a witness string in an
            |ambiguous regex, where the ambiguity is found in the left or right
            |of a Concatenation"""
    .stripMargin.replaceAll("\n", " ") in {
      val ambiguity = ((c^2).* ~ (c^4).*)  
      val r1 = Concatenate(b, ambiguity)
      val r2 = Concatenate(ambiguity, b)
      val (ambiguousSubexprRight, witnessRight) = r1.unambiguous.value
      val (ambiguousSubexprLeft, witnessLeft) = r2.unambiguous.value
      val dvm = DerivativeMachine(ambiguity)
      ambiguousSubexprRight shouldEqual ambiguity
      ambiguousSubexprLeft shouldEqual ambiguity 
      dvm.eval(witnessLeft) shouldEqual true
      dvm.eval(witnessRight) shouldEqual true
    }


  it should """return None if the regex is an unambiguous Concatenation""" in {
    val r1 = Concatenate(complex_unambiguous, d)
    val r2 = Concatenate(d, complex_unambiguous)
    r1.unambiguous shouldEqual None
    r2.unambiguous shouldEqual None
  }


  it should """find the ambiguous subexpression and a witness string in an
            |ambiguous regex, where the ambiguity is due to either overlap
            |or a nullable inner expression of a KleeneStar"""
    .stripMargin.replaceAll("\n", " ") in {

      //inner1 is not nullable but (inner1 overlap inner1.*) is not empty
      val inner1 = Concatenate(Union(b,ε), b)
      inner1.nullable shouldEqual ∅
      (inner1 overlap inner1.*).empty shouldEqual false
      val r1 = KleeneStar(inner1)
      
      //inner is nullable but (inner overlap inner.*) is empty
      val inner2 = ε 
      inner2.nullable shouldEqual ε
      (inner2 overlap inner2.*).empty shouldEqual true
      val r2 = KleeneStar(inner2)

      val (ambiguousExpr1, witness_overlap) = r1.unambiguous.value
      val (ambiguousExpr2, witness_nullable) = r2.unambiguous.value
      ambiguousExpr1 shouldEqual r1
      ambiguousExpr2 shouldEqual r2
      val dvm1 = new DerivativeMachine(r1)
      val dvm2 = new DerivativeMachine(r2)
      dvm1.eval(witness_overlap) shouldEqual true
      dvm2.eval(witness_nullable) shouldEqual true
      witness_nullable shouldEqual ""
    }

  it should """find the ambiguous subexpression and a witness string in an
            |ambiguous regex, where the ambiguity is due to an ambiguous
            |inner expression of a KleeneStar"""
    .stripMargin.replaceAll("\n", " ") in {
      val ambiguity = b.? ~ b.?
      val r = KleeneStar(ambiguity)
      val (ambiguousSubexpr, witness) = r.unambiguous.value
      ambiguousSubexpr shouldEqual ambiguity
      val dvm = new DerivativeMachine(ambiguousSubexpr)
      dvm.eval(witness) shouldEqual true
    }


  it should """return None if the regex is an unambiguous KleeneStar""" in {
    val r = KleeneStar(complex_unambiguous)
    r.unambiguous shouldEqual None
  }


  it should """find the ambiguous subexpression and a witness string in an
            |ambiguous regex, where the ambiguity is found in the child
            |of a capture"""
    .stripMargin.replaceAll("\n", " ") in {
      val r = Capture("FooBar", b.? ~ b.?)
      val (ambiguousSubexpr, witness) = r.unambiguous.value
      ambiguousSubexpr should equal (b.? ~ b.?)
      (new DerivativeMachine(ambiguousSubexpr)).eval(witness)
    }


  it should """return None if the regex is an unambiguous Capture""" in {
    val r = Capture("FooBar", complex_unambiguous)
    r.unambiguous shouldEqual None
  }


  it should """return the left-most ambiguous subexpression in a complex
               |expression which contains multiple ambiguous subexpressions"""
    .stripMargin.replaceAll("\n", " ") in {
      val r = Capture("foo", KleeneStar(
                        Concatenate(b.? ~ b.?, Union(c.? ~ c.?, d.? ~ d.?))))
      val (ambiguousSubexpr, witness) = r.unambiguous.value 
      ambiguousSubexpr should equal (b.? ~ b.?)
      val dvm = new DerivativeMachine(ambiguousSubexpr)
      dvm.eval(witness) shouldEqual true
    }


  it should """return the right-child ambiguous subexpression in a complex
               |expression which contains multiple ambiguous subexpressions,
               |and no ambiguity in its left child"""
    .stripMargin.replaceAll("\n", " ") in {
      val r = Capture("foo", KleeneStar(Concatenate(b, Union(c, d.? ~ d.?))))
      val (ambiguousSubexpr, witness) = r.unambiguous.value
      ambiguousSubexpr should equal (d.? ~ d.?)
      val dvm = new DerivativeMachine(ambiguousSubexpr)
      dvm.eval(witness) shouldEqual true
    }


  it should """return the correct ambiguous subexpression in a complex
               |expression which contains multiple ambiguous subexpressions"""
    .stripMargin.replaceAll("\n", " ") in {
      val r = Concatenate(
        Concatenate((b.? | c) ~ (b | c.?), d.? ~ d.?),
        c.? ~ c.?)
      val (ambiguousSubexpr, witness) = r.unambiguous.value
      ambiguousSubexpr should equal ((b.? | c) ~ (b | c.?))
      val dvm = new DerivativeMachine(ambiguousSubexpr)
      dvm.eval(witness) shouldEqual true
    }


  // more tests...

  it should "return None if the expression is unambiguous" in {
    val r = complex_unambiguous
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
