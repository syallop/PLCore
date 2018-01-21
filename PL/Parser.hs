{-# LANGUAGE
    InstanceSigs
  , DeriveFunctor
  , GADTs
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  #-}
{-|
Module      : PL.Parser
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A NIH parser with backtracking, leftovers and automatic whitespace consumption.
-}
module PL.Parser
  ( -- Core parser functions
    ParseResult(..)
  , Parser ()
  , runParser
  , pFail
  , pSucceed
  , req
  , sat
  , try
  , recoverWith
  , alternatives
  , labeled, (<?>)

   -- Functions on characters
  , takeChar
  , Expected(..)
  , Predicate(..)
  , takeCharIf
  , charIs

   -- Take kinds of character
  , upper
  , lower
  , digit

   -- Functions on Text/ many characters
  , takeN
  , takeNIf
  , textIs
  , takeWhile
  , takeWhile1
  , dropWhile
  , dropWhile1

   -- Misc
  , natural
  , whitespace

  , pointTo
  , remainder
  , showExpected
  , parseResult

  , Cursor(..)

  , isoMapParser
  , productMapParser
  , toParser
  ) where

import Prelude hiding (takeWhile,dropWhile,exp)

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Char
import Data.Function
import Data.Monoid
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Text as Text

import PL.Iso
import PL.Parser.Cursor
import PL.Parser.Expected
import PL.Printer.Doc
import PL.Grammar ((*/),(\*))
import qualified PL.Grammar as G


-- | A Parser is a function which takes 'Text' and either fails or produces some 'a' and some leftover 'Text'.
-- Instances for Monad & Applicative sequence Parsers together left-to-right, propogating failure.
-- Instances for MonadPlus & Alternative sequence left-to-right when successful but have backtracking behaviour on failure.
newtype Parser a = Parser {_unParser :: Cursor -> ParseResult a}

-- Add leftovers to failure?
-- - Allows recovery to work
data ParseResult a
  = ParseSuccess a        Cursor -- Parsed 'a' with leftovers
  | ParseFailure Expected Cursor -- Expected something with leftovers
  deriving (Show, Functor)

instance Document a
      => Document (ParseResult a) where
  document p = case p of
    ParseSuccess a leftovers
      -> DocText "Parsed: " <> document a <> DocText "with leftovers" <> document leftovers

    ParseFailure e c
      -> mconcat
           [DocText "Parse failure."
           ,DocText "Expected one of: "

           ,document $ mconcat [ document e
                               , lineBreak
                               , DocText "At this position in the input:"
                               , lineBreak
                               , document c
                               ]
           ]

-- | Case analysis on a 'ParseResult'.
parseResult
  :: (a -> Cursor -> b)
  -> (Expected -> Cursor -> b)
  -> ParseResult a
  -> b
parseResult sF fF r = case r of
  ParseSuccess a c -> sF a c
  ParseFailure e c -> fF e c

-- | A predicate on some 'a' also describes it's expected values.
data Predicate a
  = Predicate
    {_predicate       :: a -> Bool
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
  fmap f (Parser pa) = Parser $ \c0 -> case pa c0 of
    ParseSuccess a  c1 -> ParseSuccess (f a) c1
    ParseFailure es c1 -> ParseFailure es c1

-- Delegates to Monad instance
instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- | Consume nothing to produce the value.
  return = Parser . ParseSuccess

  -- | The Parser must succeed and may consume input. The result and remaining
  -- input is passed into f.
  (Parser pa) >>= f = Parser $ \c0 -> case pa c0 of
    ParseSuccess a c1
      -> let Parser pb = f a
            in pb c1

    ParseFailure e c1
      -> ParseFailure e c1

instance MonadFail Parser where
  fail msg = pFail $ ExpectLabel (Text.pack msg) ExpectAnything

-- If the left alternative fails but consumes input, pretend we havnt and try the right alternative
instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  -- | The zero parser fails with no expectations.
  mzero = pFail expectNothing

  -- | Try the left parser, if it fails backtrack, trying the right as if no
  -- input had been consumed.
  mplus (Parser pa0) (Parser pa1) = Parser $ \cur0 -> case pa0 cur0 of
    ParseSuccess a cur1
      -> ParseSuccess a cur1

    ParseFailure expected0 cur1
      -- Consumed no input, try the next.
      | on (==) _cursorPos cur0 cur1
       -- TODO: If this fails we might want to remember the first parse failed.
       -- As it is we can report multiple Expects either-ed together but only
       -- one cursor in the ParseFailure. This means one might point to the
       -- wrong location. We need to either tag expects with cursors or return
       -- a list of ParseFailures in the Parser.
       {--> pa1 cur1-}
       -> case pa1 cur1 of
            ParseSuccess a cur2
              -> ParseSuccess a cur2

            -- Remember the first parser was also expected.
            ParseFailure expected1 cur2
              -> ParseFailure (ExpectEither expected0 expected1) cur2

      -- Consumed input. Fail.
      | otherwise
       -> ParseFailure expected0 cur1

-- | Execute a 'Parser' on some input Text, producing a possible result and leftover Text if successful.
runParser
  :: Parser a
  -> Text
  -> ParseResult a
runParser (Parser p) txt = p (Cursor [] txt $ Pos 0 0 0)


-- | Fail without consuming anything
pFail
  :: Expected
  -> Parser a
pFail = Parser . ParseFailure

-- | Succeed without consuming anything
pSucceed
  :: Parser ()
pSucceed = Parser $ ParseSuccess ()

-- | Require a parse must succeed, but any result is discarded.
req
  :: Parser a
  -> Parser ()
req p = p >>= const pSucceed

-- | A parse must succeed and satisfy a predicate.
sat
  :: Predicate a
  -> Parser a
  -> Parser a
sat pred (Parser f) = Parser $ \cur0 -> case f cur0 of
  ParseSuccess a cur1
    | _predicate pred a -> ParseSuccess a cur1
    | otherwise         -> ParseFailure (_predicateExpect pred) cur0 -- cur1?

  failure
    -> failure

-- | Pretend no input has been consumed if a parse fails.
try
  :: Parser a
  -> Parser a
try (Parser p) = Parser $ \c -> case p c of
  ParseFailure es _
    -> ParseFailure es c

  r -> r

-- | If a parser fails, recover with the given function, continuing just after
-- the failure position.
recoverWith
  :: (Expected -> Pos -> Parser a)
  -> Parser a
  -> Parser a
recoverWith f (Parser p) = Parser $ \c -> case p c of
  ParseFailure es c1
    -> let Parser p1 = f es (_cursorPos c1)
          in p1 c1

  r -> r


-- | Try each parser in succession, backtracking on failure.
alternatives
  :: [Parser a]
  -> Parser a
alternatives = foldr (<|>) mzero

-- | Label a parser. If it fails, the label will appear in the 'Expected'
-- section of the output.
labeled
  :: Text
  -> Parser a
  -> Parser a
labeled label (Parser f) = Parser $ \c -> case f c of
  ParseFailure e c'
    -> ParseFailure (ExpectLabel label e) c'

  success
    -> success

-- | Infix 'labeled'.
(<?>) :: Parser a -> Text -> Parser a
(<?>) = flip labeled


-- | Take a single character (if there are any left).
takeChar
  :: Parser Char
takeChar = Parser $ \cur0 -> case advance cur0 of
  Nothing
    -> ParseFailure ExpectAnything cur0

  Just (cur1, c)
    -> ParseSuccess c cur1

-- Take a character that must satisfy a predicate
takeCharIf
  :: Predicate Char
  -> Parser Char
takeCharIf pred = sat pred takeChar

-- Take a character if it is equal to the one given
charIs
  :: Char
  -> Parser ()
charIs c = req $ takeCharIf (Predicate (== c) (ExpectOneOf [Text.singleton c]))

upper, lower, digit :: Parser Char
upper = takeCharIf (Predicate isUpper (ExpectPredicate "upper" Nothing)) :: Parser Char
lower = takeCharIf (Predicate isLower (ExpectPredicate "lower" Nothing)) :: Parser Char
digit = takeCharIf (Predicate isDigit (ExpectPredicate "digit" Nothing)) :: Parser Char

-- | Take a number of chars, if the input is long enough.
takeN
  :: Int
  -> Parser Text
takeN i
  = Parser $ \cur0
              -> maybe (error "Can't take negative characters")
                       (\(cur1, txt) -> ParseSuccess txt cur1) $ advanceN i cur0


-- | Take a number of chars if the resulting text passes a predicate.
takeNIf
  :: Predicate Text
  -> Int
  -> Parser Text
takeNIf pred i = sat pred $ takeN i


-- | Take a string of text.
textIs
  :: Text
  -> Parser ()
textIs fullTxt = Parser $ \cur0 -> textIs' fullTxt cur0
  where
    textIs' :: Text -> Cursor -> ParseResult ()
    textIs' txt cur0 = case Text.uncons txt of
      Nothing
        -> ParseSuccess () cur0

      Just (t,ts)
        -> case advance cur0 of
             -- End of input but we still need a character
             Nothing
               -> ParseFailure (ExpectLabel ("In text" <> fullTxt) $ ExpectOneOf [Text.cons t ts]) cur0

             Just (cur1, c)
               | c == t    -> textIs' ts cur1
               | otherwise -> ParseFailure (ExpectLabel ("In text"<>fullTxt) $ ExpectOneOf [Text.cons t ts]) cur1

-- | Take the longest text that matches a predicate on the characters.
-- Possibly empty.
takeWhile
  :: (Char -> Bool)
  -> Parser Text
takeWhile pred = Parser $ \cur0 -> let (cur1,txt) = advanceWhile pred cur0
                                    in ParseSuccess txt cur1

-- | Takewhile, but must take at least one character => not the empty text.
takeWhile1
  :: Predicate Char
  -> Parser Text
takeWhile1 pred = Parser $ \cur0 -> case advanceWhile1 (_predicate pred) cur0 of
  Nothing
    -> ParseFailure (_predicateExpect pred) cur0

  Just (cur1, txt)
    -> ParseSuccess txt cur1

-- | Drop the longest text that matches a predicate on the characters.
-- Possibly empty.
dropWhile
  :: (Char -> Bool)
  -> Parser ()
dropWhile = req . takeWhile

-- | Drop the longest text that matches a predicate on the characters.
-- Must succeed on at least one character.
dropWhile1
  :: Predicate Char
  -> Parser ()
dropWhile1 = req . takeWhile1


-- | A natural number: zero and positive integers
natural
  :: Parser Int
natural = read . Text.unpack <$> takeWhile1 (Predicate isDigit $ ExpectPredicate "ISNATURAL" Nothing)

-- | Consume whitespace.
whitespace
  :: Parser ()
whitespace  = dropWhile isSpace

-- A Parser that accepts that grammar if the Iso also succeeds.
isoMapParser
  :: Show a
  => Iso a b
  -> G.Grammar a
  -> Parser b
isoMapParser iso gr =
  let Parser p = toParser gr
   in Parser $ \cur0 -> case p cur0 of
        ParseSuccess a cur1
          -> case parseIso iso a of
               Nothing
                 -> ParseFailure (ExpectLabel "isoMap" $ grammarExpects gr) cur0 -- cur1?

               Just b
                 -> ParseSuccess b cur1

        ParseFailure e cur1
          -> ParseFailure e cur1



-- | Tuple the result of two successive parsers.
productMapParser :: Parser a -> Parser b -> Parser (a,b)
productMapParser fa fb = (,) <$> fa <*> fb

-- | Convert a Grammar to a Parser that accepts it.
toParser :: G.Grammar a -> Parser a
toParser grammar = case grammar of
  -- A single character if one is available.
  G.GAnyChar
    -> takeChar

  -- A token of text stopping at the first whitespace.
  G.GAnyText
    -> takeWhile1 (Predicate (not . isSpace) $ ExpectAnything)

  -- Return the value.
  G.GPure a
    -> pure a

  -- Fail with no Expectations.
  G.GEmpty
    -> empty

  -- If the left fails, try the right as if no input had been consumed.
  G.GAlt g0 g1
    -> toParser g0 <|> toParser g1

  -- Parse the grammar if the iso succeeds.
  G.GIsoMap iso ga
    -> isoMapParser iso ga

  -- | Tuple the result of two successive parsers.
  G.GProductMap ga gb
    -> productMapParser (toParser ga) (toParser gb)

  -- Enhance a failing parse with a given Expect label.
  G.GLabel l g
    -> let Parser f = toParser g
        in Parser $ \cur0 -> case f cur0 of
             ParseFailure e cur1
               -> ParseFailure (ExpectLabel l e) cur1
             s -> s

  G.GTry g0
    -> try . toParser $ g0
