{- |
Module: Helpers.Parsing
Description: A Quick Parsing Library

One of the things I found sweatiest about working Advent Of Code in Haskell was
actually just parsing the input.  Constantly dealing with the Maybes/Eithers
and other potential failures felt like a quiz on Monad operators, rather than
the fun puzzles I wanted to be doing.  So I spent a couple days building out
this library for parsing.

== Vocab
In this library, I'm working from the following vocab:

=== Nouns:
  * Token: A recognized piece of data.
  * ScanResult:  A collection (often a sequence) of tokens
  * Scanner: A function that turns a string into a result

=== Verbs:
  * Tokenize: Turn a string into a single token. 
  * Scan: Turn a a string into a result
  * Read: Extract a value from a token
  * Grok: Turn a result into a useful type
  * Parse: Use a specific scanner to comprehend a string and turn it into a
  useful type.

== Usage

For this example, we're going to parse a line that has rectangles with a name...
something like:
@
Rupert: x=3..5, y=17...33
@

=== Target Type
I recommend first defining the type you'd like to parse to.  Wether you're
planning to parse all the input at once or do so line by line.  It doesn't
really matter how you define it, as long as it exists.

@
    data NamedRectangle = NamedRectangle String Int Int Int Int
@

=== Define a Scanner
The next step is to define your 'Scanner'.  See "Scanning" below for how to build
a 'Scanner'.  The 'Scanner' for this situation is:

@
    scanRect = (/=\':\') ^& ": x=" ^& scanInt ^& ".." ^& scanInt ^& ", y=" ^& scanInt ^& ".." ^& scanInt
@

but for Brevity and readability's sake we might want to do something like this:

@
    scanRect = (/=\':\') ^& ": x=" ^& range ^& ", y=" ^& range
        where range = scanInt ^& ".." ^& scanInt
@
    

=== Define a function
Now, for convenience, define a function that takes the same types you'll see
in the input /in the same order/ and returns your target type (or a result of 
your target type).  You can use this in concert with the family of 'grok'
functions to automatically grab your tokens out of a scan result and put them
into your function.  In our case, our constructor already does everything
we need.

=== Create an instance of 'Grokkable' for your Target Type
You only need one function for 'Grokkable' - `fromResult`.  You can use the 'grok'
function with the right number of expected parameters to define it:

@
    instance Grokkable NamedRectangle
        toResult = grok5 NamedRectangle
@

Note - if your function returns a Result, then use compose 'Control.Monad.join' with 'grok'
to collapse the Results.

=== Pass your 'Scanner' into parse
Once you've done all that, you can use the parse function in conjunction with
your 'Scanner' to produce a function that can parse your input into your target type:

@
    parseRect :: String -> Result NamedRectangle
    parseRect = parse scanRect
@

It's important to note that 'parse' can actually attempt to generate any
'Grokkable' type, so it will need a type hint to create the right type.

== Scanning

Scanners greedily consume from a string, and then yield a ScanResult which may 
contain one or more tokens, and may contain an unscanned "remainder".  It's thus
best to think of scanners in terms of exactly what they will consume and produce.

=== Basic Scanners

There are several basic scanners buit in - see functions like `scanInt`, `consume`,
or `remember`.  They consume from the beginning of the string, and typically
produce a maximum of one token.  For instance, if each line of your input is
an integer, you can likely get away with `scanInt` as your scanner.

=== Combining Scanners
There are three major methods of combining your scanners, `sequential`, `alternating`,
and `repeating`.  Those functions explain their use.  They also have equivalent
operators to let you write very succint scanners.  For instance, if you want to
parse a list of elves names and ages, you might use:

@
    scanElf = (scanStr (/=' ')) ^& (consume " ") ^& scanInt
@

=== Scannables
To help you be even more succinct, we've added transformations from several
common types into Scanners.  A @Char => Bool@ will always be turned into
a `scanStr` scanner, a @String@ into a `consume` scanner, etc.  This can
help the above example look like:

@
    scanElf = (/=' ') ^& " " ^& scanInt
@

== A Complete Example.
Imagine an input where you need to parse out Elf basketball teams, listed like this:

@
The Present Wrappers: Snowball is 60cm, Cocoa is 72cm, Jingle is 59cm, Jurgen is 80cm, Scranthus is 63 cm
@

The full parse step might look like:

@
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
@       
 

-}
module Helpers.Parsing (
    -- * Types
    Token(..),
    Tokenizable(..),
    ScanResult,
    Scanner,
    Scannable(..),
    ReadableFromToken(..),
    Grokkable(..),
    -- * Scanners #scanners#
    -- ** Built In Scanners
    scanChar,
    scanInt,
    -- ** Parametric Scanners
    consume,
    remember,
    chomp,
    scanStr,
    -- ** Indicators and Assertions
    end,
    indicatedBy,
    indicatedByEmptyList,
    -- * Combining Scanners
    discard,
    -- ** Explicit
    sequential,
    alternating,
    repeating,
    -- ** Operators
    (^&),
    (^|),
    (^*),
    -- * Parsing
    parse,
    -- ** Working with ScanResults
    get,
    grok,
    grok2,
    grok3,
    grok4,
    grok5,
    grok6,
    grok7,
) where

import Data.List (isPrefixOf)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import GHC.Data.Maybe(firstJusts, orElse)
import Lib.Types(Result)

{-|
The result of applying a Scanner to a String.  This acts kind of like a
list of tokens - with some specific error handling and "rest of computation"
pieces.
-}
data ScanResult = Failure String -- ^ A Failed Scan, with a message about why
    | Empty -- ^ A successful scan, with no resulting tokens
    | Remainder String -- ^ The un-scanned portion of a String.
    | Scanned Token ScanResult  -- ^ A successful scan with a single token, and
                                -- a recursive scanResult
    deriving Show

{-|
This class is used in combinators to convert more succint representations of
input into working Scanners.
-}
class Scannable a where
    scan :: a -> Scanner

-- | create a "Consume" scanner.
instance Scannable String where
    scan = consume

-- | Creates a "scanStr"
instance Scannable (Char -> Bool) where
    scan = scanStr

-- | Scan a string of a specific length
instance Scannable Int where
    scan = chomp

-- | Identity
instance Scannable Scanner where
    scan = id
-- | Creates an "end" scanner.
instance Scannable () where
    scan = const end

{-|
A function that takes in a string, and attempts to break it into tokens.
-}
type Scanner = String -- ^ Your input
    -> ScanResult

{- |
Represents a single token in a ScanResult.  
-}
data Token = Ok {-^ A match which stores nothing -}
    | IntTok Integer -- ^ An Integer
    | StrTok String -- ^ A String
    | CharTok Char -- ^ A Single Character
    | RepTok [ScanResult] -- ^ Every individual result of a repitition.
    deriving Show

{-|
This class is used exclusively by "indicatedBy" to inject tokens into
ScanResults
-}
class Tokenizable a where
    toToken :: a -> Token

-- | Create a String Token
instance Tokenizable String where
    toToken = StrTok

-- | Create an Int Token
instance Tokenizable Integer where
    toToken = IntTok

-- | Create a Reptition Token
instance Tokenizable [ScanResult] where
    toToken = RepTok

-- | Create a Char Token
instance Tokenizable Char where
    toToken = CharTok

{-|
Scan for a positive or negative integer.  This is not whitespace tolerant.
-}
scanInt :: Scanner
scanInt haystack = case intScanner haystack of
    Scanned (StrTok m) (Scanned (StrTok d) rest) -> case readMaybe (m ++ d) of
        Just n -> Scanned (IntTok n) rest
        Nothing -> Failure ("Not a legal integer: " ++ m ++ d)
    Scanned (StrTok s) rest -> case readMaybe s of
        Just n -> Scanned (IntTok n) rest
        Nothing -> Failure ("Not a legal integer: " ++ s)
    other -> other

{-|
Scan for a single character.
-}
scanChar :: Scanner
scanChar "" = Failure "Reached end of String"
scanChar (c:rest) = Scanned (CharTok c) $ Remainder rest

-- ** Parametric Scanners

{-|
Match an exact string, and then discard.  This is great for delimiters and
whitespace.
-}
consume :: String -- ^ The String to skip oer.
        -> Scanner
consume needle haystack
    | needle `isPrefixOf` haystack = Scanned Ok $ Remainder $ drop (length needle) haystack
    | otherwise = Failure $ "Could not consume: " ++ needle

{-|
Match an exact string, and provide it as a token in the result.  Especially
when combined with alternation, this is excellent for recognizing enumerations.
-}
remember :: String -- ^ The string to look for.
    -> Scanner
remember needle haystack
   | needle `isPrefixOf` haystack = Scanned (StrTok needle) $ Remainder $ drop (length needle) haystack
   | otherwise = Failure $ "Could not consume: " ++ needle

{-|
Take a string of a known length.  This will fail if there are not enough characters.
-}
chomp :: Int -> Scanner
chomp l haystack
    | length chomped == l = Scanned (StrTok chomped) $ Remainder rest
    | otherwise = Failure $ "Needed " ++ show l ++ " characters, found " ++ show (length chomped)
    where (chomped, rest) = splitAt l haystack
{-|
Scan a string one character at a time, as long as the function returns true.
This method is irrevocably greedy.
-}
scanStr :: (Char -> Bool) -> Scanner
scanStr f haystack = Scanned (StrTok left) (Remainder right)
    where (left, right) = span f haystack

-- | Combine two scannables sequentially
(^&) :: (Scannable a, Scannable b) => a -> b -> Scanner
l ^& r = sequential [scan l, scan r]

-- | Combine two scannables in alternation
(^|) :: (Scannable a, Scannable b) => a -> b -> Scanner
l ^|  r = alternating [scan l, scan r]

-- | Combine two scannables into a repeating scanner.  See @repeating@.
(^*) :: (Scannable a, Scannable b)
    => a -- ^ The repeating unit
    -> b -- ^ The delimiter
    -> Scanner
target ^* delimiter  = repeating (scan delimiter) (scan target)


{-|
Take any scanner, and discard any tokens it finds.
-}
discard :: Scannable a => a -> Scanner
discard s = getRemainder . scan s
    where getRemainder (Scanned _ rest) = getRemainder rest
          getRemainder x = x

{-|
Combine a list of scannables sequentially, i.e.: scan the first,
then the second, then the third, etc.  If one fails, they all fail.
-}
sequential :: Scannable a => [a] -> Scanner
sequential [] s = Remainder s
sequential [scannable] s = scan scannable s
sequential (scannable:rest) s = scanRemaining (scan scannable s) $ sequential rest

{-|
Combine a list of scannables in alternation.  Try each of them from the same
point in the string, then choose the first successful one.  This alternation
is not backtrackable
-}
alternating :: Scannable a => [a] -> Scanner
alternating scannables haystack = case success of
        (r:_) -> r
        _ -> Failure "No Successful Alternations"
    where success = filter isSuccessful . map (($ haystack) . scan) $ scannables

{-|
Repeat a scannable, with a delimiter.  The delimiter must appear between any
repititions. (If a delimiter is not desired, try consuming an empty string).
Delimiters are _dropped_, and not available in the final ScanResult.  The
repeater will happily consume a trailing delimiter.

Repeating scanners create repition tokens, which can only be consumed as
arrays of a `Grokkable` type.
-}
repeating :: (Scannable a, Scannable b) => a -> b -> Scanner
repeating delimiter target haystack = if isSuccessful firstMatch
        then Scanned (RepTok matches) (Remainder remainder)
        else Scanned (RepTok []) (Remainder haystack)
    where matches = firstMatch:otherMatches
          remainder = firstJusts [afterRepitition, afterDelimiter, afterFirstMatch] `orElse` haystack
          (firstMatch, afterFirstMatch) = terminate $ scan target haystack
          (_, afterDelimiter) = case afterFirstMatch of
                Just s -> terminate $ scan delimiter s
                Nothing -> (Empty, Nothing)
          (otherMatches, afterRepitition) = case repeating delimiter target <$> afterDelimiter of
                Just (Scanned (RepTok t) (Remainder r)) -> (t, Just r)
                _ -> ([], Nothing)

{-|
Sometimes it's valuable to be able to indicate a match by a token.  In this
situation, you can inject one directly via this method.
-}
indicatedBy :: Tokenizable a
    => a -- The token you would like to inject.
    -> Scanner
indicatedBy tok s = Scanned (toToken tok) $ Remainder s

{-|
Empty lists are tokenizable, but it requires a very clunky typehint.  This
convenience function wraps up that typehint for you.
-}
indicatedByEmptyList :: Scanner
indicatedByEmptyList = indicatedBy ([] :: [ScanResult])

{-|
Assert that the end of the input has been reached, or fail.
-}
end :: Scanner
end "" = Empty
end s = Failure ("Not at the end: " ++ s)

{-|
Use a scanner and an instance of Grokkable to parse a string. Importantly,
it collapses out unreadable tokens, and makes positional information more
consistent.
-}
parse :: Grokkable a
    => Scanner -- Your Scanner
    -> String -- Your Input
    -> Result a
parse scanner str = fromResult =<< collapse (scanner str)

{-|
This class allows reading from a token to recover type information.
-}
class ReadableFromToken a where
    readTok :: Token -> Result a

-- | Read a string from a StrTok
instance ReadableFromToken String where
    readTok (StrTok s) = Right s
    readTok o = toLeft "String" o

-- | Read an Integer from an IntTok
instance ReadableFromToken Integer where
    readTok (IntTok i) = Right i
    readTok o = toLeft "Integer" o

-- | Read a Char from a CharTok
instance ReadableFromToken Char where
    readTok (CharTok c) = Right c
    readTok o = toLeft "Char" o

instance (ReadableFromToken a, ReadableFromToken b) => ReadableFromToken (Either a b) where
    readTok t = fromTrys (readTok t) (readTok t)
        where fromTrys _ (Right r) = Right . Right $ r
              fromTrys (Right l) _ = Right . Left $ l
              fromTrys (Left e) (Left e2) = Left $ "Tried but: " ++ e ++ " or " ++ e2

{-|
Allow parsing from a ScanResult into an Either String a.  This is
used inside `parse`.
-}
class Grokkable a where
    fromResult :: ScanResult -> Result a

instance {-# OVERLAPPABLE #-} ReadableFromToken a => Grokkable a where
    fromResult = get 0

-- | Any token can be read into a Maybe - failures just become Nothing
instance {-# OVERLAPPABLE #-} ReadableFromToken a => Grokkable (Maybe a) where
    fromResult t = Right (case get 0 t of
        Right o -> Just o
        _ -> Nothing)

-- | All Grokkables can be read out of Repitition tokens as lists.
instance {-# OVERLAPPABLE #-} Grokkable a => ReadableFromToken [a] where
    readTok (RepTok results) = mapM fromResult results
    readTok o = toLeft "Repitition" o

{-|
Get the nth token out of a ScanResult.  If the type doesn't match, this
will return a Left with a message about it.
-}
get :: ReadableFromToken a => Int -> ScanResult -> Result a
get _ (Failure s) = Left $ "Reached a failure: " ++ s
get _ Empty = Left "Not enough parsed tokens"
get _ (Remainder _) = Left "Not Enough Parsed Tokens"
get n (Scanned Ok rest) = get n rest
get 0 (Scanned t _) = readTok t
get n (Scanned _ rest) = get (n - 1) rest

{-|
The grok family of functions, like the `Control.Applicative.liftA` family of functions, allow you to
fill a function's arguments with results from a Scan and retrieve a Result
out the other side.
-}
grok :: ReadableFromToken a
    => (a -> b)
    -> ScanResult
    -> Result b
grok f r = f <$> get 0 r

-- | grok for 2 arguments
grok2 :: ReadableFromToken a
    => ReadableFromToken b
    => (a -> b -> c)
    -> ScanResult
    -> Result c
grok2 f r = f <$> get 0 r <*> get 1 r

-- | grok for 3 arguments
grok3 :: ReadableFromToken a
    => ReadableFromToken b
    => ReadableFromToken c
    => (a -> b -> c  -> h)
    -> ScanResult
    -> Result h
grok3 f r = f <$> get 0 r <*> get 1 r <*> get 2 r

-- | grok for 4 arguments
grok4 :: ReadableFromToken a
    => ReadableFromToken b
    => ReadableFromToken c
    => ReadableFromToken d
    => (a -> b -> c -> d -> h)
    -> ScanResult
    -> Result h
grok4 f r = f <$> get 0 r <*> get 1 r <*> get 2 r <*> get 3 r

-- | grok for 5 arguments
grok5 :: ReadableFromToken a
    => ReadableFromToken b
    => ReadableFromToken c
    => ReadableFromToken d
    => ReadableFromToken e
    => (a -> b -> c -> d -> e -> h)
    -> ScanResult
    -> Result h
grok5 f r = f <$> get 0 r <*> get 1 r <*> get 2 r <*> get 3 r <*> get 4 r

-- | grok for 6 arguments
grok6 :: ReadableFromToken a
    => ReadableFromToken b
    => ReadableFromToken c
    => ReadableFromToken d
    => ReadableFromToken e
    => ReadableFromToken f
    => (a -> b -> c -> d -> e -> f -> h)
    -> ScanResult
    -> Result h
grok6 f r = f <$> get 0 r <*> get 1 r <*> get 2 r <*> get 3 r <*> get 4 r <*> get 5 r

-- | grok for 7 arguments
grok7 :: ReadableFromToken a
    => ReadableFromToken b
    => ReadableFromToken c
    => ReadableFromToken d
    => ReadableFromToken e
    => ReadableFromToken f
    => ReadableFromToken g
    => (a -> b -> c -> d -> e -> f -> g -> h)
    -> ScanResult
    -> Result h
grok7 f r = f <$> get 0 r <*> get 1 r <*> get 2 r <*> get 3 r <*> get 4 r <*> get 5 r <*> get 6 r


------------
-- Utilities:
-------------
{-|
Scan something that _looks_ like a string.  This will either yield a "-" token
followed by a String containing exclusively digits, or it will yield no token
at all.  This is intended primarily for internal use - `scanInt` will read these
tokens into a single Int token.
-}
intScanner :: Scanner
intScanner = alternating [sequential [remember "-", scanStr isDigit], scanStr isDigit]

{-|
This is an internal utility to eliminate empty tokens and make ScanResults
more consistently scannable.
-}
collapse :: ScanResult -> Result ScanResult
collapse (Scanned Ok rest) = collapse rest
collapse (Scanned (RepTok results) rest) = Scanned <$> rep <*> collapse rest
    where rep = RepTok <$> mapM collapse results
collapse (Scanned tok rest) = Scanned tok <$> collapse rest
collapse other = Right other

{-|
Internal convenience function for turning an unexpected token type into a
useful message about type mismatch.
-}
toLeft :: String -> Token -> Result a
toLeft expected t = Left $ "Expected " ++ expected ++ " but found " ++ actual ++ " instead!"
    where actual = case t of
                    (StrTok s) -> "String: " ++ show s
                    (IntTok i) -> "Integer: " ++ show i
                    (CharTok c) -> "Char: " ++ show c
                    Ok -> "Non Capturing Token"
                    (RepTok _) -> "Repetition"

{-|
This internal utility is used for repitition, to retrieve the remaining unscanned
strings.
-}
terminate :: ScanResult -> (ScanResult, Maybe String)
terminate (Failure s) = (Failure s, Nothing)
terminate Empty = (Empty, Nothing)
terminate (Remainder s) = (Empty, Just s)
terminate (Scanned tok rest) = (Scanned tok rest', remainder)
    where (rest', remainder) = terminate rest

{-|
This runs a scanner against the unscanned string in a result - failing if
there is nothing to read.
-}
scanRemaining :: ScanResult -> Scanner -> ScanResult
scanRemaining (Remainder s) scanner = scanner s
scanRemaining (Scanned tok res) scanner = Scanned tok (scanRemaining res scanner)
scanRemaining Empty _ = Failure "Nothing left to Scan"
scanRemaining x _ = x -- Failure, chiefly.

{-|
Check for any failure tokens in the ScanResult Chain.
-}
isSuccessful :: ScanResult -> Bool
isSuccessful (Scanned _ rest) = isSuccessful rest
isSuccessful (Failure _) = False
isSuccessful _ = True
