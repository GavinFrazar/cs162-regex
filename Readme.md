# Assignment 2

## Deadline: October 25, 11:59PM

This week in class we learned how to do string recognition with derivatives, and went in detail through a simple recursive implementation. We also talked about taking a different approach and implementing a stack-based virtual machine for computing derivatives.

In this assignment, you will be implementing:
  - Convenient regex builder class using the "pimp my library" pattern, with regex simplifications
  - A virtual machine for computing derivatives of regular expressions
  - Unit tests for your code

## Project structure

The repo includes the following:

  - `src/main/scala/Derive.scala` - contains a recursive implementation of derivative parsing we developed in class. Use this as a reference to check the behavior of your VM implementation. *Do not change this file*
  - `src/main/scala/RangeSet.scala` - implements dense sets based on intervals. *Do not change this file*
  - `src/main/scala/Regex.scala` - contains the abstract syntax for regular expressions, as well as toString and pretty printing methods. *Do not change this file*
  - `src/main/scala/Util.scala` - contains some useful utilities. Feel free to add things to this file if you need to.
  - `src/main/scala/RichRegex.scala` - contains the skeleton code for a rich regex builder library. *You should implement all unimplemented methods in this file*
  - `src/main/scala/DerivativeMachine.scala` - contains the skeleton code for VM-based derivative matching. *You should implement all unimplemented methods in this file*

  `src/test/scala/*` - Contains skeleton code for unit tests. *You should implement your own tests in this directory*

## Part 1: Rich regex builder & simplifications

The rich builder library allows the user to define regular expressions in a more natural and convenient way. For example, instead of writing `Concatenate(Union(re1, re2), re3)`, we will be able to write `(re1 ~ re2) | re3`.

Aside from just constructing regular expressions, the rich builder class also needs to implement simplifications over regular expressions (much like you did in the first assignment).

Below is a list of operations you need to implement, accompanied by the description of simplifications for each operation. For each operation, you will attempt to perform a simplification first, if applicable.

Notation: 
  - Regular expressions will be labeled with `r`, `r1`, `r2`
  - Sets of characters will be labeled with `{a}`, `{b}`, `{c}`
  - `∅` and `ε` denote the empty language and the empty string respectively
  - `α` denotes a language accepting any single character, or (equivalently) the set of all characters in the alphabet

### Concatenation: `re1 ~ re2`

The `~` operator should create a concatenation of two regular expressions. If applicable, the following simplifications must be applied:

  - `r ~ ∅ => ∅`
  - `∅ ~ r => ∅`
  - `r ~ ε => r`
  - `ε ~ r => r`

In other words, concatenating anything with the empty language results in an empty language. Concatenating a language with the empty string results in the original language. If no simplifications can be applied, `r1 ~ r2` should return `Concatenate(r1, r2)`.

### Union: `r1 | r2`

The `|` operator returns a union of two regular expressions.

Simplifications :
  - `r | ∅ => r`
  - `∅ | r => r`
  - `{a} | {b} => {a ∪ b}`
  - `r* | ε  => r*`
  - `ε  | r* => r*`
  - `α* | r  => α*`
  - `r  | α* => α*`
  - `r | r => r`

The third rule says that a union of two regular expressions consisting only of character sets, can be simplified into a single regular expression accepting the union of both character sets. In other words, `{a, b} | {c, d}` can be simplified to `{a, b, c, d}`.

In the fourth rule, the left side of the union already accepts the empty string, so the right side is redundant.

The sixth and seventh rules say that if one side of the union matches any string (`α*`), then the other side of the union is redundant, whatever it is.

The last rule says that the union of two regular expressions that are identical can be reduced to either one.

**TIP:** When matching on values with names like `ε` or `α`, remember to put them in backticks (`` `α` ``), otherwise the compiler will think you are attempting to bind the value to a variable (which will always succeed).

### Kleene star: `re*`

The postfix `*` operator returns a Kleene star of `re`.

Simplifications:

  - `∅* => ε`
  - `ε* => ε`
  - `(r*)* => r*`

### Complement: `!re`

  - `!∅ => α*`
  - `!ε => α+`

The other methods do not require simplifications.

### Intersection: `re1 & re2`

The `&` operator returns an intersection of two languages.

Simplifications:

  - `∅ & r => ∅`
  - `r & ∅ => ∅`
  - `{a} & {b} => {a ∩ b}`
  - `α* & r => r`
  - `r & α* => r`
  - `r & r => r`

## Part 2: Derivative machine

You need to implement the virtual machine for computing derivatives, as talked about in class. You will implement the following methods:

  1. `run` method which executes a given program (sequence of
       instructions), and returns the top of the final operand stack.
  2. `derive` method which returns the derivative of the regular expression by calling run with `PushDerive` and the given character.
  3. `eval` method which checks if `re` recognizes `str` by taking successive derivations.

Use the recursive implementation as a guide, your implementation should do exactly the same thing but be tail recursive.

## Part 3: Tests

You will need to write your own unit tests. Testing your own code *is part of the grade*. We are looking for the following:
  - Non-trivial tests. Ie. a test that demonstrates that creating a charset creates a charset is not very useful.
  - Decent coverage - we don't expect you to go for a 100% test coverage, but we do expect you to cover edge cases.

## Rules

You must use only the purely functional subset of Scala. This means that you are not allowed to use mutations; more explicitly you must not use any of:
  - Mutable variables, i.e., those created using the `var` keyword,
  - Mutable collections, e.g. anything under `scala.collection.mutable`,
  - The `Array` data type.

If you use any mutation, you will automatically fail the assignment.

Your code must compile. Invoking `compile` and `test:compile` in the SBT shell (as described in the first tutorial) must succeed. Otherwise, you will automatically fail the assignment.

Good luck!
