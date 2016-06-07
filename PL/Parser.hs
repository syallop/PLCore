{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module PL.Parser
  (-- Core parser functions
   ParseResult(..)
  ,Parser()
  ,runParser
  ,pFail
  ,pSucceed
  ,req
  ,sat
  ,try
  ,recoverWith
  ,alternatives

   -- Functions on characters
  ,takeChar
  ,takeCharIf
  ,charIs

  -- Take kinds of character
  ,upper
  ,lower
  ,digit

  -- Match kinds of character
  ,space
  ,arrow
  ,bar
  ,star
  ,plus
  ,comma
  ,upArrow
  ,lambda
  ,langle
  ,rangle
  ,lparen
  ,rparen
  ,underscore
  ,union
  ,question
  ,at

  -- Functions on Text/ many characters
  ,takeN
  ,takeNIf
  ,textIs
  ,takeWhile
  ,takeWhile1
  ,dropWhile
  ,dropWhile1

  -- Misc
  ,natural
  ,between
  ,betweenParens
  ,whitespace
  ) where

import Prelude hiding (takeWhile,dropWhile,exp)

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Char

-- | A Parser is a function which takes 'Text' and either fails or produces some 'a' and some leftover 'Text'.
-- Instances for Monad & Applicative sequence Parsers together left-to-right, propogating failure.
-- Instances for MonadPlus & Alternative sequence left-to-right when successful but have backtracking behaviour on failure.
-- Both instances implicity consume any trailing whitespace after a successful parse.
newtype Parser a = Parser {_unParser :: Cursor -> ParseResult a}

data Pos
  = Pos
    {_posTotal    :: Int -- Total number of characters into a parse

    ,_posLine     :: Int -- Number of new lines
    ,_posLineChar :: Int -- Number characters into line
    }
  deriving Show

-- A cursor is a position within some text, where we remember how much text we've passed,
-- how many newlines and how much into the current line we are but not the prior text itself
data Cursor = Cursor
  {_cursorText :: Text
  ,_cursorPos  :: Pos
  }
  deriving Show

-- Increment a number of character along a line
incAlongLine :: Int -> Pos -> Pos
incAlongLine i (Pos t l s) = Pos (t+i) l (s+i)

-- Increment to a new line, reseting to position zero within the line.
-- A newline character takes up one total character.
incLine :: Int -> Pos -> Pos
incLine i (Pos t l s) = Pos (t+1) (l+1) 0

-- Increment a position by a string of Text moved past
incPast :: Text -> Pos -> Pos
incPast txt p = case Text.uncons txt of
  Nothing       -> p
  Just (c,txt') -> incPast txt' $ incPastChar c p

incPastChar :: Char -> Pos -> Pos
incPastChar c
  | c == '\n' = incLine 1
  | otherwise = incAlongLine 1

-- Increment the Cursor past the next character (if there is one), returning it
incCursor :: Cursor -> Maybe (Char,Cursor)
incCursor (Cursor txt pos) = do
  (c,txt') <- Text.uncons txt
  Just . (c,) . Cursor txt' $ case c of
     _
      | c == '\n' -> incLine      1 pos
      | otherwise -> incAlongLine 1 pos


-- Add leftovers to failure?
-- - Allows recovery to work
data ParseResult a
  = ParseSuccess a        Cursor -- Parsed 'a' with leftovers
  | ParseFailure Expected Cursor -- Expected something with leftovers
  deriving Show

data Expected
  = ExpectEither Expected Expected -- Expected either of
  | ExpectOneOf [Text]             -- Expected any of
  | ExpectPredicate Text           -- Failed predicate with label
  | ExpectAnything                 -- Expected anything => got an EOF
  | ExpectN Int Expected           -- Expected a N repetitions

expectNothing :: Expected
expectNothing = ExpectOneOf []

instance Show Expected where
  show e = case e of
    ExpectEither es0 es1
      -> "{" ++ show es0 ++ "} {" ++ show es1 ++ "}"

    ExpectOneOf ts
      -> "Expected " ++ showOneOf ts

    ExpectPredicate t
      -> "predicate " ++ Text.unpack t

    ExpectAnything
      -> "Expected ANYTHING"

    ExpectN i e
      -> "Expected exactly " ++ show i ++ " of " ++ show e

showOneOf :: [Text] -> String
showOneOf []       = "NOTHING"
showOneOf (x:[])   = Text.unpack x
showOneOf (x:y:[]) = Text.unpack x <> "," <> Text.unpack y
showOneOf (x:y:zs) = Text.unpack x <> "," <> Text.unpack y <> "," <> showOneOf zs


data Predicate a
  = Predicate
    {_predicate       :: (a -> Bool)
    ,_predicateExpect :: Expected -- What does the predicate expect? Could fall back to a simple label
    }

instance Monoid a => Monoid (Parser a) where
  mempty = return mempty
  mappend pa pa' = do
    a  <- pa
    a' <- pa'
    return (a <> a')

-- fmap over successfully parsed values
instance Functor Parser where
  fmap f (Parser pa) = Parser $ \c -> case pa c of
    ParseSuccess a  c' -> ParseSuccess (f a) c'
    ParseFailure es c' -> ParseFailure es c'

instance Applicative Parser where
  pure  = return
  (<*>) = ap
instance Monad Parser where
  return = Parser . ParseSuccess

  (Parser pa) >>= f = Parser $ \c -> case pa c of
    ParseFailure es c1
      -> ParseFailure es c1

    ParseSuccess a c1
      -> let Parser pb = f a
            in case pb (dropSpaceLikes c1) of
                 ParseFailure es c2
                   -> ParseFailure es c2

                 ParseSuccess b c2
                   -> ParseSuccess b (dropSpaceLikes c2)

-- If the left alternative fails but consumes input, pretend we havnt and try the right alternative
instance Alternative Parser where
  empty = mzero
  (<|>) = mplus
instance MonadPlus Parser where
  mzero = pFail expectNothing

  mplus (Parser pa0) (Parser pa1) = Parser $ \c0 -> case pa0 c0 of
    ParseFailure es0 _
      -> case pa1 c0 of
           ParseFailure es1 c1
             -> ParseFailure (ExpectEither es0 es1) c1

           ParseSuccess a1 c1
             -> ParseSuccess a1 (dropSpaceLikes c1)

    ParseSuccess a0 c1
      -> ParseSuccess a0 (dropSpaceLikes c1)

-- Given a position within some Text, advance the position by dropping any space
-- like characters until the first non-space character.
dropSpaceLikes :: Cursor -> Cursor
dropSpaceLikes (Cursor txt p) = case Text.uncons txt of
  Nothing -> Cursor txt p
  Just (c,txt')
    | c == ' '  -> Cursor txt' $ incAlongLine 1 p
    | c == '\n' -> Cursor txt' $ incLine 1 p
    | otherwise -> Cursor txt p

-- | Execute a 'Parser' on some input Text, producing a possible result and leftover Text if successful.
runParser :: Parser a -> Text -> ParseResult a
runParser (Parser p) txt = p (Cursor txt $ Pos 0 0 0)


-- | Fail without consuming anything
pFail :: Expected -> Parser a
pFail = Parser . ParseFailure

-- | Succeed without consuming anything
pSucceed :: Parser ()
pSucceed = Parser $ ParseSuccess ()

-- | Require a parse must succeed, but any result is discarded
req :: Parser a -> Parser ()
req p = p >>= \_ -> pSucceed

-- | A parse must suceed and satisfy a predicate.
sat :: Predicate a -> Parser a -> Parser a
sat pred p = p >>= \a -> if _predicate pred $ a then return a else pFail (_predicateExpect pred)

-- | Pretend no input has been consumed if a parse fails
try :: Parser a -> Parser a
try (Parser p) = Parser $ \c -> case p c of
  ParseFailure es _
    -> ParseFailure es c

  r -> r

-- If a parser fails, recover with the given function, continuing just after the failure position
recoverWith :: (Expected -> Pos -> Parser a) -> Parser a -> Parser a
recoverWith f (Parser p) = Parser $ \c -> case p c of
  ParseFailure es c1
    -> let Parser p1 = f es (_cursorPos c1)
          in p1 c1

  r -> r


-- | Try each parser in succession, backtracking on failure.
alternatives :: [Parser a] -> Parser a
alternatives []     = pFail expectNothing
alternatives (p:ps) = p <|> alternatives ps



-- Take a single character (if there are any left that is..)
takeChar :: Parser Char
takeChar = Parser $ \c -> case Text.uncons (_cursorText c) of
  Nothing -> ParseFailure ExpectAnything c
  Just (a,txt) -> ParseSuccess a (Cursor txt (incPastChar a (_cursorPos c)))

-- Take a character that must satisfy a predicate
takeCharIf :: Predicate Char -> Parser Char
takeCharIf pred = sat pred takeChar

-- Take a character if it is equal to the one given
charIs :: Char -> Parser ()
charIs c = req $ takeCharIf (Predicate (== c) (ExpectOneOf [Text.singleton c]))




upper = takeCharIf (Predicate isUpper (ExpectPredicate "isUpper")) :: Parser Char
lower = takeCharIf (Predicate isLower (ExpectPredicate "isLower")) :: Parser Char
digit = takeCharIf (Predicate isDigit (ExpectPredicate "isDigit")) :: Parser Char


space      = req $ takeCharIf (Predicate isSpace (ExpectOneOf [" "]))
arrow      = textIs "->"
bar        = charIs '|'
star       = charIs '*'
plus       = charIs '+'
comma      = charIs ','
upArrow    = charIs '^'
lambda     = charIs '\\'
langle     = charIs '<'
rangle     = charIs '>'
lparen     = charIs '('
rparen     = charIs ')'
underscore = charIs '_'
union      = charIs 'U'
question   = charIs '?'
at         = charIs '@'

-- number of characters until one is a space or a newline
spaceLikeDistance :: Text -> Int
spaceLikeDistance txt = spaceLikeDistance' (0,txt)
  where
    spaceLikeDistance' :: (Int,Text) -> Int
    spaceLikeDistance' (accD,txt) = case Text.uncons txt of
      Nothing -> accD
      Just (c,txt')
        | c == ' ' || c == '\n' -> accD
        | otherwise             -> spaceLikeDistance' (accD+1,txt')

-- Is the given length less than, equal or greater than the distance to the next
-- space or newline?
compareLengthUntilSpaceLike :: Int -> Text -> Ordering
compareLengthUntilSpaceLike i txt
  | i < 0     = LT
  | otherwise = compareLengthUntilSpaceLike' i txt
  where
    compareLengthUntilSpaceLike' :: Int -> Text -> Ordering
    compareLengthUntilSpaceLike' 0 txt = case Text.uncons txt of
      Nothing                   -> EQ
      Just (c,txt')
        | c == ' ' || c == '\n' -> EQ
        | otherwise             -> LT
    compareLengthUntilSpaceLike' n txt = case Text.uncons txt of
      Nothing                   -> GT
      Just (c,txt')
        | c == ' ' || c == '\n' -> GT
        | otherwise             -> compareLengthUntilSpaceLike' (n-1) txt'


-- | Take a number of chars, if the input is long enough
takeN :: Int -> Parser Text
takeN i
  | i < 0     = error "Can't take a negative number of characters"
  | i == 0    = return ""
  | otherwise = Parser $ \c -> case compareLengthUntilSpaceLike i (_cursorText c) of

    -- Less than the required number of characters until a space
    LT -> ParseFailure (ExpectN i ExpectAnything) c
    _  -> case Text.splitAt i (_cursorText c) of
            (a,txt) -> ParseSuccess a (Cursor txt (incPast a (_cursorPos c)))


-- Take a number of chars if the resulting text passes a predicate
takeNIf :: Predicate Text -> Int -> Parser Text
takeNIf pred i = sat pred $ takeN i


-- Take a string of text
textIs :: Text -> Parser ()
textIs t = req $ takeNIf (Predicate (== t) (ExpectOneOf [t])) (Text.length t)


-- Take the longest text that matches a predicate on the characters
-- (possibly empty)
takeWhile :: (Char -> Bool) -> Parser Text
takeWhile pred = Parser $ \c -> case Text.span pred (_cursorText c) of
  (a,txt) -> ParseSuccess a $ Cursor txt $ incPast a $ _cursorPos c

-- Takewhile, but must take at least one character => not the empty text
takeWhile1 :: Predicate Char -> Parser Text
takeWhile1 pred = Parser $ \c -> case Text.span (_predicate pred) (_cursorText c) of
  ("",_)  -> ParseFailure (_predicateExpect pred) c
  (a,txt) -> ParseSuccess a $ Cursor txt $ incPast a $ _cursorPos c

-- Drop the longest text that matches a predicate on the characters
-- (possibly empty)
dropWhile :: (Char -> Bool) -> Parser ()
dropWhile = req . takeWhile

-- | Drop the longest text that matches a predicate on the characters.
-- Must succeed on at least one character.
dropWhile1 :: Predicate Char -> Parser ()
dropWhile1 = req . takeWhile1


-- A natural number: zero and positive integers
natural :: Parser Int
natural = read . Text.unpack <$> takeWhile1 (Predicate isDigit $ ExpectPredicate "isDigit")

-- | Discard the result of two wrapping parsers.
between :: Parser l -> Parser a -> Parser r -> Parser a
between pl pa pr = pl *> pa <* pr

-- | Run a parser between two parenthesis '(' ')'.
betweenParens :: Parser a -> Parser a
betweenParens pa = between lparen pa rparen

whitespace :: Parser ()
whitespace  = dropWhile isSpace

