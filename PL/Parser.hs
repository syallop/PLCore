{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module PL.Parser
  (-- Core parser functions
   ParseResult(..)
  ,Parser()
  ,runParser
  ,pFail
  ,pSucceed
  ,req
  ,sat
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
newtype Parser a = Parser {_unParser :: Text -> ParseResult a}

data ParseResult a
  = ParseSuccess a Text   -- Parsed 'a' with leftovers
  | ParseFailure Expected -- List of alternate expected strings
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
      -> showOneOf ts

    ExpectPredicate t
      -> "predicate " ++ Text.unpack t

    ExpectAnything
      -> "Expected ANYTHING"

    ExpectN i e
      -> "Expected " ++ show i ++ " of " ++ show e

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

instance Functor Parser where
  fmap f (Parser pa) = Parser $ \txt -> case pa txt of
    ParseFailure es -> ParseFailure es
    ParseSuccess a txt' -> ParseSuccess (f a) txt'

instance Applicative Parser where
  pure  = return
  (<*>) = ap
instance Monad Parser where
  return a = Parser $ \txt -> ParseSuccess a txt

  (Parser pa) >>= f = Parser $ \txt -> case pa txt of
    ParseFailure es
      -> ParseFailure es

    ParseSuccess a txt'
      -> let Parser pb = f a
            in case pb (Text.dropWhile isSpace txt') of
                 ParseFailure es
                   -> ParseFailure es

                 ParseSuccess b txt'
                   -> ParseSuccess b (Text.dropWhile isSpace txt')

--
instance Alternative Parser where
  empty = mzero
  (<|>) = mplus
instance MonadPlus Parser where
  mzero = pFail expectNothing

  mplus (Parser pa0) (Parser pa1) = Parser $ \txt -> case pa0 txt of
    ParseFailure es0
      -> case pa1 txt of
           ParseFailure es1
             -> ParseFailure (ExpectEither es0 es1)

           ParseSuccess a1 txt'
             -> ParseSuccess a1 (Text.dropWhile isSpace txt')

    ParseSuccess a0 txt'
      -> ParseSuccess a0 (Text.dropWhile isSpace txt')

-- | Execute a 'Parser' on some input Text, producing a possible result and leftover Text if successful.
runParser :: Parser a -> Text -> ParseResult a
runParser (Parser p) txt = p txt


-- | Fail without consuming anything
pFail :: Expected -> Parser a
pFail = Parser . const . ParseFailure

-- | Succeed without consuming anything
pSucceed :: Parser ()
pSucceed = Parser $ \txt -> ParseSuccess () txt

-- | Require a parse must succeed, but any result is discarded
req :: Parser a -> Parser ()
req p = p >>= \_ -> pSucceed

-- | A parse must suceed and satisfy a predicate.
sat :: Predicate a -> Parser a -> Parser a
sat pred p = p >>= \a -> if _predicate pred $ a then return a else pFail (_predicateExpect pred)

-- | Try each parser in succession, backtracking on failure.
alternatives :: [Parser a] -> Parser a
alternatives []     = pFail expectNothing
alternatives (p:ps) = p <|> alternatives ps



-- Take a single character (if there are any left that is..)
takeChar :: Parser Char
takeChar = Parser $ \txt -> case Text.uncons txt of
  Nothing -> ParseFailure ExpectAnything
  Just (a,txt') -> ParseSuccess a txt'

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



-- | Take a number of chars, if the input is long enough
takeN :: Int -> Parser Text
takeN i
  | i < 0     = error "Cant take a negative number of characters"
  | i == 0    = return ""
  | otherwise = Parser $ \txt -> case Text.compareLength txt i of
      LT -> ParseFailure (ExpectN i ExpectAnything)
      _  -> case Text.splitAt i txt of
              (a,txt') -> ParseSuccess a txt'

-- Take a number of chars if the resulting text passes a predicate
takeNIf :: Predicate Text -> Int -> Parser Text
takeNIf pred i = sat pred $ takeN i


-- Take a string of text
textIs :: Text -> Parser ()
textIs t = req $ takeNIf (Predicate (== t) (ExpectOneOf [t])) (Text.length t)


-- Take the longest text that matches a predicate on the characters
-- (possibly empty)
takeWhile :: (Char -> Bool) -> Parser Text
takeWhile pred = Parser $ \txt -> case Text.span pred txt of
  (a,txt') -> ParseSuccess a txt'

-- Takewhile, but must take at least one character => not the empty text
takeWhile1 :: Predicate Char -> Parser Text
takeWhile1 pred = Parser $ \txt -> case Text.span (_predicate pred) txt of
  ("",_)   -> ParseFailure (_predicateExpect pred)
  (a,txt') -> ParseSuccess a txt'

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
