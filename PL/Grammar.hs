{-# LANGUAGE
    GADTs
  , RankNTypes
  , OverloadedStrings
  #-}
{-|
Module      : PL.Grammar
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

A description of a grammar. The intent is that it can be used as either a
parser or a printer depending on context. This could prevent round-trip
properties from being accidentally violated for example, by adjusting a parser 
but not the printer.

Note: WIP. Currently only suitable for translation to a Parser (and maybe not
even that). Some constructors may be missing/ some may be unneccessary and in
general the entire type is likely to change.
-}
module PL.Grammar
  ( Grammar()
  , charIs
  , textIs

  , anyChar
  , charWhen

  , upper
  , lower
  , digit

  , arrow
  , bar
  , star
  , plus
  , comma
  , upArrow
  , lambda
  , langle
  , rangle
  , lparen
  , rparen
  , underscore
  , union
  , question
  , at
  , bigLambda
  , bigAt

  , between
  , betweenParens

  , longestMatching
  , longestMatching1

  , natural

  , alternatives
  )
  where

import Data.Text (Text)
import Data.Char
import qualified Data.Text as T
import Data.Foldable
import Data.Monoid
import Control.Applicative
import PL.Parser (Parser)
import qualified PL.Parser as P

-- | The grammar of some language.
data Grammar a where
  -- Literal text
  GText
    :: Text
    -> Grammar ()

  -- Any character
  GAnyChar
    :: Grammar Char

  -- A matching character
  GCharWhen
    :: P.Predicate Char
    -> Grammar Char

  -- Longest matching text
  GLongestMatching
    :: (Char -> Bool)
    -> Grammar Text

  -- Longest matching text. At least one character.
  GLongestMatching1
    :: P.Predicate Char
    -> Grammar Text

  -- Monoid mempty
  GMEmpty
    :: Monoid a
    => Grammar a

  -- Monoid <>
  GMappend
    :: Monoid a
    => Grammar a
    -> Grammar a
    -> Grammar a

  -- Functor fmap
  GMap
    :: (b -> a)
    -> Grammar b
    -> Grammar a

  -- Applicative pure
  GPure
    :: a
    -> Grammar a

  -- Applicative ap
  GAp
    :: Grammar (b -> a)
    -> Grammar b
    -> Grammar a

 -- Alternative empty
  GEmpty
    :: Grammar a

  -- Alternative <|>
  GAlt
    :: Grammar a
    -> Grammar a
    -> Grammar a

   -- Monad bind
  GBind
    :: Grammar b
    -> (b -> Grammar a)
    -> Grammar a

instance Foldable Grammar where
  foldMap f gr = fold $ fmap f gr

instance Monoid a => Monoid (Grammar a) where
  mempty = GMEmpty
  mappend = GMappend

instance Functor Grammar where
  fmap = GMap

instance Applicative Grammar where
  pure = GPure
  (<*>) = GAp

instance Alternative Grammar where
  empty = GEmpty
  (<|>) = GAlt

instance Monad Grammar where
  return = pure
  (>>=) = GBind

-- | Convert a Grammar to a Parser that accepts it.
toParser :: Grammar a -> Parser a
toParser grammar = case grammar of
  GText txt
    -> P.textIs txt

  GAnyChar
    -> P.takeChar

  GCharWhen pred
    -> P.takeCharIf pred

  GLongestMatching p
    -> P.takeWhile p

  GLongestMatching1 pred
    -> P.takeWhile1 pred

  GMEmpty
    -> mempty

  GMappend g0 g1
    -> toParser g0 <> toParser g1

  GMap f g0
    -> fmap f . toParser $ g0

  GPure a
    -> pure a

  GAp g0 g1
    -> toParser g0 <*> toParser g1

  GEmpty
    -> empty

  GAlt g0 g1
    -> toParser g0 <|> toParser g1

  GBind g0 f
    -> toParser g0 >>= toParser . f

-- | A single character.
charIs :: Char -> Grammar ()
charIs = GText . T.singleton

-- | A string of Text.
textIs :: Text -> Grammar ()
textIs = GText

-- | Any single character
anyChar :: Grammar Char
anyChar = GAnyChar

-- | A character that matches a predicate
charWhen :: P.Predicate Char -> Grammar Char
charWhen = GCharWhen

upper :: Grammar Char
upper = charWhen (P.Predicate isUpper (P.ExpectPredicate "ISUPPER"))

lower :: Grammar Char
lower = charWhen (P.Predicate isLower (P.ExpectPredicate "ISLOWER"))

digit :: Grammar Char
digit = charWhen (P.Predicate isDigit (P.ExpectPredicate "ISDIGIT"))

arrow      = charIs '→' <|> textIs "->"
bar        = charIs '|'
star       = charIs '*'
plus       = charIs '+'
comma      = charIs ','
upArrow    = charIs '^'
lambda     = charIs 'λ' <|> charIs '\\'
langle     = charIs '<'
rangle     = charIs '>'
lparen     = charIs '('
rparen     = charIs ')'
underscore = charIs '_'
union      = charIs '∪'
question   = charIs '?'
at         = charIs '@'
bigLambda  = charIs 'Λ' <|> textIs "/\\"
bigAt      = charIs '#'

-- | A Grammar between two others.
between :: Grammar l -> Grammar a -> Grammar r -> Grammar a
between l a r = l *> a <* r

-- | A Grammar between parentheses.
betweenParens :: Grammar a -> Grammar a
betweenParens a = between lparen a rparen

-- | Longest matching text.
longestMatching :: (Char -> Bool) -> Grammar Text
longestMatching = GLongestMatching

-- | Longest matching text. At least one character.
longestMatching1 :: P.Predicate Char -> Grammar Text
longestMatching1 = GLongestMatching1

-- | A natural number: zero and positive integers
natural :: Grammar Int
natural = read . T.unpack <$> longestMatching1 (P.Predicate isDigit $ P.ExpectPredicate "ISNATURAL")

-- | A list of alternative Grammars.
alternatives :: [Grammar a] -> Grammar a
alternatives = foldr (<|>) GEmpty

