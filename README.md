# Advent of Haskell

I really love Haskell, and I think you could love it, too.  Please feel free to
use this framework for Advent of Code to try and give yourself a leg up to
learning Haskell.  The goals for this framework are:

* Allow you to engage in Haskell without learning the IO Monad
* Give you an opinionated starting point for structuring your programs
* Give you access to easy command line invocations for running your programs

## Setup

### Install Haskell and Cabal

The Haskell ecosystem is surprisingly fragmented - your exact system will make
the exact path you take to installing haskell unique to you.  However, I
recommend that you pursue a toolchain that will allow you to install multiple
versions of Haskell, as this repository is tested on the following versions:

* Haskell: 9.4.3
* Cabal: 3.10

I have had the most success using [asdf](https://asdf-vm.com/) to install Haskell,
and downloading the [cabal](https://www.haskell.org/cabal/download.html) binary
directly.

I believe that [GHCup](https://www.haskell.org/ghcup/) has a high likelihood of
of success as well, but am unfortunately not an expert in Haskell installation.

You have a working installation when you can run `cabal build` in the root of this
repository without getting error messages.

#### Optional: Install Hlint

You can use hlint to learn even more about haskell best practices - I highly
recommend it.

```bash
cabal install hlint
```

### Configure Make

The Makefile in this project will automatically download the input for a given
day if you configure it properly.  This requires two files, `.year` and `.cookie`.
Both of these are marked in the `.gitignore` so you will not accidentally commit
them.

#### .year

This file is just the 4 digit year on which you're working.  Initializing this
is as easy as 

```bash
cat 2023 > .year
```

#### .cookie

This is your `session` cookie for Advent of Code.  I'm not going to walk you through
getting it, because I don't want to cast too wide a net and accidentally get
someone.  But this file should look something like:

```
session=<long hexadecimal number>
```

Putting your login cookie in a file on your machine is... not a safe practice in
general.  I promise none of my code is leveraging it, but if you don't feel right
about it, don't do it.  You can skip the Makefile entirely - I will provide the 
direct cabal commands as well.

## Running the Code

You can use `cabal` or `make` to run your code.  `make` will require a session
cookie, but `cabal` will require you to download the input yourself.

### Make

#### Running the tests for a given day

```bash
make test_<day>_<part>
```

The make target takes an integer for the day.  For Part, you can use `1`, `2`, or
`both`.  For example:

```bash
make test_16_2
```

Will run the tests for Day 16, Part 2.

#### Solving the puzzle for a given day

```bash
make puzzle_<day>_<part>
```

The day and part are exactly the same as for the tests.  When solving the puzzle,
the framework will _first_ run against all the configured tests.  If your tests fail,
it will refuse to run on the larger input, to save you time.

#### Running all puzzles

At the end of AoC, I always like to see all my answers run at once.  It's just
a fun thing, so I baked it in.

```bash
make all
```

#### Fix all lint warnings

```bash
make lint-fix
```

### Cabal

#### Running the tests for a given day

```bash
cabal run AdventOfCode -- test <day> <part>
```

As with Make, you can put the day as an integer, and the part as 1, 2, or "both".

#### Solving the puzzle for a given day

```bash
cabal run AdventOfCode -- puzzle <day> <part>
```

#### Running all puzzles

```bash
cabal run AdventOfCode -- puzzle all
```

#### Fix all lint warnings

Not technically cabal, but I include `doLint.sh` for the makefile,
so you can just call that directly.

```bash
./doLint.sh
```

## Solving Problems

Okay, to the meat of the repository - how do you actually solve Advent of Code 
problems with this Repository?

### Extract examples

The examples in the text of the puzzles can be extracted out into their own inputs,
additionally, you can make your own!  Store them as flat text files in the
[examples directory](examples) for the appropriate day.  You can name the files
whatever you like - extension does not matter.  There is _one_ reserved input
name: `input`, where the day's given puzzle input is expected to be.

The filename here is used to map test inputs to expected outputs, and also in the
output of the run, to let you know what went wrong, so be descriptive, but avoid
spaces.

### Configure tests

The solve file for that day and problem are in `problems/Day<day>/Part<part>.hs`
(for instance, [Day One, Part One](problems/DayOne/PartOne.hs)).  You'll find two
things you should change immediately:

* *Out* is a type alias for your output type.  Replace it with whatever the
output of the puzzle is (usually Int, Integer, or String)
* *examples* is a list of test case tuples - the first element is the string
name of the file (no directory, just filename and extension), and the second is
the output that should be produced.

These tests will run before any attempt to calculate the main input, and serve
to build confidence in your solution before some of the more calculation heavy
main inputs.

### Write a Parser

One of the opinions I bring to the table about Advent of Code is this: You should
parse your input into a real data structure before you start trying to actually
solve the problem. To that end, you should write a parser, which takes in a
String (the input file contents), and outputs a parsed representation of the
input.  Sometimes the type is as simple as `[[Int]]`, but sometimes it's much
more complex.

Check out the included [Helpers](helpers/helpers.md), especially the relatively
full featured [parsing library](helpers/helpers.md#helpersparsing) that has been
included for help writing this function.

This parser is the second parameter to the `adventOfCode` function at the bottom
of the file.

### Write a Solver

Next, and most fun (hopefully), write a solver, which takes the output of your
parser, and calculates the solution!  This solver is the last argument to the
`adventOfCode` function at the bottom of the file.

### Notes

#### Error Handling

It's considered fairly poor practice to throw an exception in Haskell.  Instead,
if your function might fail in some way, you're expected to make that clear in
the return type.

This framework uses the `Result` type to do that, which is a homegrown type
alias for `Either String`.  Parsers and Solvers are _both_ expected to leverage it.
If your function cannot fail, use the
[always](helpers/helpers.md#always-and-alwaysconf) helper to coerce.  If you
need to manage your own failure, just remember to:

* return `Right output` when you've got an answer
* return `Left "A Descriptive Error Message"` when you encounter a failure.

#### Adding Dependencies

[Hoogle](https://hoogle.haskell.org/) is an incredible resource for looking up
libraries and functions in haskell.  But not everything there is in the standard
library.  If you want to add dependencies to your build, do so in
[AdventOfCode.cabal](AdventOfCode.cabal), under the build-depends for the library
"problems".  This will automatically make the appropriate libraries available
for import in your problem code. There's a comment in there to help you
find your way.

#### Debug Logging

It's a crutch, but we love it.  Sometimes it's nice to be able to see what your
functions are _doing_, and in what order.  This can be _really_ hard to do
in a non-imperative language like haskell.  So, I wanted to call out my preferred
method of doing so, the
[`Debug.Trace`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Debug-Trace.html)
module - and specifically the
[`trace`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Debug-Trace.html#v:trace)
function.  All the functions in this package work the same way - they take
something printable, and something to return (sometimes the same thing), then
print the printable thing to stderr, then return the thing you wanted to return.

This allows you to ouput information without having to change the signature of a
function, or having to deal with the IO Monad.
