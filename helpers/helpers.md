# Helpers

Here are some helpers to make your problem solving a bit more ergonomic.  This document assumes
you've read the similar one in the `lib` folder, as well as the Global README.

## Helpers.Solution

Functions to help you scaffold out your solution. Import via

```haskell
import Helpers.Solution
```

in order to be able to call these functions by name. If you encounter name collisions, you can
namespace with

```haskell
import qualified Helpers.Solution as Solution
```

in order to be able to call the functions like `Solution.always`

### nyi and nyiConf

This stands for not yet implemented.  It consumes input and always returns an error what's not been
implemented. Use like:

```haskell
import Helpers.Solution

solution = adventOfCode examples (nyi "Parsing" :: String -> Result Out) (nyi "Solution")
```

for non-configurable problems. or like:

```haskell
import Helpers.Solution

solution = adventOfCodeConfigurable examples 3 (nyi "Parsing" :: String -> Result Out) (nyiConf "Solution")
```

for configurable problems.

### always and alwaysConf

These functions are convenience functions to help you if solutions (or parsers) do not need error
states.  For instance, say all you need to do is sum some numbers as part of your solution, you 
could do that with:

```haskell
import Helpers.Solution

sum :: [Integer] -> Integer
sum = foldl (+) 0

getTotalHeight :: [Integer] -> Result Integer
getTotalHeight = always sum
```

The configurable version passes through both arguments to your function.  Consider a configurable
solution where you need to muliply a list and use some configured base:

```haskell
import Helpers.Solution

product :: Integer -> [Integer] -> Integer
product = foldl (*)

getHyperVolume :: Integer -> [Integer] -> Result Integer
getHyperVolume = alwaysConf product
```

### maybeToResult

Listen, a lot of libraries default to maybe, but this repo encourages you to use Results so you 
get readable error messages.  Unpacking a maybe isn't _hard_ per se, but it does clutter a bit,
so I've included this helper to just do it for you.  Provide it an error message and your maybe.

```haskell
import Distribution.Simple.Utils
import Helpsers.Solution

getLeadElf :: [String] -> Result String
getLeadElf elves = maybeToResult "No Lead Elf Candidate" leadElf
    where leadElf = safeHead . sort . filter longEnough $ elves
          longEnough m = (length m) > 10
```

## Helpers.Input

Functions to help you parse your input.  Import via

```haskell
import Helpers.Input
```

in order to be able to call these functions by name. If you encounter name collisions, you can
namespace with

```haskell
import qualified Helpers.Input as Input
```

in order to be able to call the functions like `Input.lineByLine`

### stripNewLines
If you're worried about newlines - use this convenience function to eliminate them from your input.

### lineByLine and lineByLineM

These functions allow you to write a parser that parses a _single line_ of input, and produces
the correct output type.  Then, these functions help break up the full input from disk, run your
function against each line, and collect the results in a list.  Imagine an input where each lines
represents an elf basketball team by neame.  Something like:
```
Buddy,Happy,Stampy,Tinker,Tinsel
Figgy,Starry,Bubbles,Pixie,Jazzy
```

Define a function like:

```haskell
import Data.List.Split

names :: String -> [String]
names line = splitOn "," line
```

You can then apply lineByLine to get your actual parser:

```haskell
import Helpers.Input

parseTeams :: String -> [[String]]
parseTeams = lineByLine names
```

If there's some uncertainty in your parsing, return a Result and use lineByLineM.  Imagine instead
of names, the teams are expressed via height in inches:

```
22,26,30,25,26
27,30,31,25,25
```

define your parser like this:

```haskell
import Text.Read
import Helpers.Solution
import Helpers.Input

heights :: String -> Result [Integer]
heights = sequence . map readHeight . splitOn ","
    where readHeight h = maybeToResult ("Could not parse \"" ++ h ++"\"") $ readMaybe h

parseTeams :: String -> Result [[Integer]]
parseTeams = lineByLineM heights
```

### charByChar and charByCharM

Like the lineByLine family, but only one character at a time.  Again, if you have uncertainty,
return a Result and use the monadic version.  Because characters are so broad, the uncertain version
is more common.  Assume you're asked to parse an Elf credit card number and do error correction on
it. (Elf Credit Cards are enormous hexadecimal numbers)

```haskell
import Data.Char
import Helpers.Input

parseCCDigit :: Char -> Result Int
parseCCDigit c
  | isHexDigit c = Right . digitToInt $ c
  | otherwise = Left $ "Not a recognized hexadecimal digit: '" ++ [c] ++ "'"

parseCC :: String -> Result [Int]
parseCC :: charByCharM parseCCDigit
```

### Combining charByChar and lineByLine

Sometimes (frequently), there's a need to parse a 2d arrangement of glyphs that have some larger
meaning.  To do this, you can use thes functions together and only have to parse one character at a
time.  Imagine a map of a landscape with directions (`^`, `v`, `<` and `>`) and blank spots (` `).

```
>>>>>v
^    v
^    v
^    v
^<<<<<
```

you could parse this like:

```haskell
import Helpers.Input

data Tile = LeftTile | RightTile | UpTile | DownTile | Blank

parseTile :: Char -> Result Tile
parseTile '<' = Right LeftTile
parseTile '>' = Right RightTile
parseTile '^' = Right UpTile
parseTile 'v' = Right DownTile
parseTile ' ' = Right Blank
parseTile c = Left $ "Unrecognized Tile: '" ++ [c] ++ "'"

parseMap :: String -> Result [[Tile]]
parseMap = (lineByLineM . charByCharM $ parseTile)
```

## Helpers.Parsing

I've reproduce the module documentation written in the code here - for help with individual
functions please check the code itself.

One of the things I found sweatiest about working Advent Of Code in Haskell was
actually just parsing the input.  Constantly dealing with the Maybes/Eithers
and other potential failures felt like a quiz on Monad operators, rather than
the fun puzzles I wanted to be doing.  So I spent a couple days building out
this library for parsing.

### Vocab
In this library, I'm working from the following vocab:

#### Nouns:
* Token: A recognized piece of data.
* ScanResult:  A collection (often a sequence) of tokens
* Scanner: A function that turns a string into a result

#### Verbs:
* Tokenize: Turn a string into a single token. 
* Scan: Turn a a string into a result
* Read: Extract a value from a token
* Grok: Turn a result into a useful type
* Parse: Use a specific scanner to comprehend a string and turn it into a useful type.

### Usage

For this example, we're going to parse a line that has rectangles with a name...
something like:

```
Rupert: x=3..5, y=17...33
```

#### Target Type
I recommend first defining the type you'd like to parse to.  Wether you're
planning to parse all the input at once or do so line by line.  It doesn't
really matter how you define it, as long as it exists.

```haskell
data NamedRectangle = NamedRectangle String Int Int Int Int
```

#### Define a Scanner
The next step is to define your 'Scanner'.  See "Scanning" below for how to build
a 'Scanner'.  The 'Scanner' for this situation is:

```haskell
scanRect = (/=\':\') ^& ": x=" ^& scanInt ^& ".." ^& scanInt ^& ", y=" ^& scanInt ^& ".." ^& scanInt
```

but for Brevity and readability's sake we might want to do something like this:

```haskell
scanRect = (/=\':\') ^& ": x=" ^& range ^& ", y=" ^& range
    where range = scanInt ^& ".." ^& scanInt
```
    

#### Define a function
Now, for convenience, define a function that takes the same types you'll see
in the input /in the same order/ and returns your target type (or a result of 
your target type).  You can use this in concert with the family of 'grok'
functions to automatically grab your tokens out of a scan result and put them
into your function.  In our case, our constructor already does everything
we need.

#### Create an instance of 'Grokkable' for your Target Type
You only need one function for 'Grokkable' - `fromResult`.  You can use the 'grok'
function with the right number of expected parameters to define it:

```haskell
instance Grokkable NamedRectangle
    toResult = grok5 NamedRectangle
```

Note - if your function returns a Result, then use compose 'Control.Monad.join' with 'grok'
to collapse the Results.

#### Pass your 'Scanner' into parse
Once you've done all that, you can use the parse function in conjunction with
your 'Scanner' to produce a function that can parse your input into your target type:

```haskell
parseRect :: String -> Result NamedRectangle
parseRect = parse scanRect
```

It's important to note that 'parse' can actually attempt to generate any
'Grokkable' type, so it will need a type hint to create the right type.

### Scanning

Scanners greedily consume from a string, and then yield a ScanResult which may 
contain one or more tokens, and may contain an unscanned "remainder".  It's thus
best to think of scanners in terms of exactly what they will consume and produce.

#### Basic Scanners

There are several basic scanners buit in - see functions like `scanInt`, `consume`,
or `remember`.  They consume from the beginning of the string, and typically
produce a maximum of one token.  For instance, if each line of your input is
an integer, you can likely get away with `scanInt` as your scanner.

#### Combining Scanners
There are three major methods of combining your scanners, `sequential`, `alternating`,
and `repeating`.  Those functions explain their use.  They also have equivalent
operators to let you write very succint scanners.  For instance, if you want to
parse a list of elves names and ages, you might use:

```haskell
scanElf = (scanStr (/=' ')) ^& (consume " ") ^& scanInt
```

#### Scannables
To help you be even more succinct, we've added transformations from several
common types into Scanners.  A `Char => Bool` will always be turned into
a `scanStr` scanner, a `String` into a `consume` scanner, etc.  This can
help the above example look like:

```haskell
scanElf = (/=' ') ^& " " ^& scanInt
```

### A Complete Example.
Imagine an input where you need to parse an Elf basketball teams, listed like this:

```
The Present Wrappers: Snowball is 60cm, Cocoa is 72cm, Jingle is 59cm, Jurgen is 80cm, Scranthus is 63cm
```

The full parse step might look like:

```
    teamNameScanner = "The " ^& (/=\':\') ^& ": "

    elfScanner = (/=' ') ^& " is " ^& scanInt ^& "cm"

    teamScanner = teamNameScanner ^& (elfScanner ^* ", ")

    data Elf = Elf String Int
    
    data Team = Team String [Elf]

    instance Grokkable Elf where
        toResult = grok2 Elf

    instance Grokkable Team where
        toResult = grok2 Team

    parseTeam = parse teamScanner

    parseInputToTeams = lineByLineM parseTeam
```       
