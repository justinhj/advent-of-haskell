# Lib
These libraries power the framework, so you largely _shouldn't_ need to interact with them.  The
Notable exceptions are Lib.Types and Lib.Solution, which I will attempt to introduce here.

This document assumes you've read the [main README](../README.md).

## "Configurable"

Most Advent of code problems require the application of the _exact_ same function on the input
as on the examples.  But sometimes, in order to make things more clear, the problems will do things
like reduce the number of rounds that are played out, or even walk a _single_ example through 
multiple rounds.  To maximally leverage the examples given, this framework introduces the concept
of "configurability" - which is essentially just an extra argument that you can ask for in your 
solver.  Many functions have a "Configurable" variant, which helps them generalize from one input
to two.

## Lib.Types

These are the base types that are used to try and lend concreteness to some of the abstract uses of
haskell's types.

The types that should matter to you are:

### Result
Result is a shorthand for the
[Either](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Either) type -
specifically `Either String`.  The Either type forms a disjoint union of two types, and is often
used to capture error messages.  By convention Left is the error value, and Right is the successful
operation.  You'll also see this called "Try" in other languages.  Because it is helpful and
convenient to _fix_ the Left type, while letting the Right type vary, I have chosen to do so in
this framework.  Use Either's constructors (`Left` and `Right`) directly to construct a Result.

### Parser

A Parser takes a single String (the entire contents of the file), and returns an arbitrary data type
representing the contents of that file.  See the [parsing helpers](../helpers/helpers.md)
for more information.

### Solver and SolverConfigurable

Solvers are functions that take an arbitrary data type (or two in the case of SolverConfigurable),
and return the solution to the problem.  This is _usually_ an integer or a String in AdventOfCode.

There are some [solution helpers](../helpers/helpers.md) available to make these more fluent.

### TestCase and TestCaseConfigurable

This is just a shorthand for the tuples used to enumerate test cases.  Remember that for
standard solutions, the test case is just `(Filename, ExpectedResult)`.  For configurable problems,
it's `(Filename, ConfigurationValue, ExpectedResult)`.

## Lib.Solution

This library contains the two backbone functions, which create a testing harness for your solutions.

### adventOfCode

This is for standard, non-configurable problems.  The parameters, in order, are:

* A List of TestCases (See above)
* A Parser (See above)
* A Solver (See above)

This is what helps abstract away reading the appropriate files, selecting between tests-only and
full runs, and gate-ing on passing tests.

### adventOfCodeConfigurable

This is the same harness, but for configurable problems.  The parameters are:

* A List of TestCases (See above)
* The configuration value to be delivered with the main input.
* A Parser (See above)
* A SolverConfigurable (See above)
