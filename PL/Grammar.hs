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

Correspondance to invertable-syntax:
  - <$>: GIsoMap:     \$/
  - <*>: GProductMap: \*/
  - <|>: GAlt:        \|/
  - empty: GEmpty
  - pure: GPure
  - token: GAnyChar:  anyChar
-}
module PL.Grammar
  ( Grammar(..)
  , toParser

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

  , seqR
  , seqL
  , (\$/)
  , (\*/)
  , (\|/)
  , (\*)
  , (*/)
  , grammarMany
  , grammarMany1
  )
  where

import Data.Text (Text)
import Data.Char
import qualified Data.Text as T
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad
import PL.Parser (Parser)
import qualified PL.Parser as P

import qualified PL.Printer as D
import PL.Printer (Doc)

import PL.Iso
import Prelude hiding ((.),id)
import Control.Category

-- | The grammar of some language.
data Grammar a where
  -- Any character
  GAnyChar
    :: Grammar Char

  -- Longest matching text
  GLongestMatching
    :: (Char -> Bool)
    -> Grammar Text

  -- Longest matching text. At least one character.
  GLongestMatching1
    :: P.Predicate Char
    -> Grammar Text

  -- Applicative-like pure
  GPure
    :: Eq a
    => a
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

  GIsoMap
    :: Iso a b
    -> Grammar a
    -> Grammar b

  GProductMap
    :: Grammar a
    -> Grammar b
    -> Grammar (a,b)

-- | Convert a Grammar to a Parser that accepts it.
toParser :: Grammar a -> Parser a
toParser grammar = case grammar of
  GAnyChar
    -> P.takeChar

  GLongestMatching p
    -> P.takeWhile p

  GLongestMatching1 pred
    -> P.takeWhile1 pred

  GPure a
    -> pure a

  GAp g0 g1
    -> toParser g0 <*> toParser g1

  GEmpty
    -> empty

  GAlt g0 g1
    -> toParser g0 <|> toParser g1

  GIsoMap iso ga
    -> P.isoMapParser iso (toParser ga)

  GProductMap ga gb
    -> P.productMapParser (toParser ga) (toParser gb)

toPrinter :: Grammar a -> D.Printer a
toPrinter grammar = case grammar of
  GAnyChar
    -> D.anyCharPrinter

  {-GLongestMatching p-}
    {--> P.takeWhile p-}

  {-GLongestMatching1 pred-}
    {--> P.takeWhile1 pred-}

  GPure a
    -> D.purePrinter a

  {-GAp g0 g1-}
    {--> toParser g0 <*> toParser g1-}

  GEmpty
    -> D.emptyPrinter

  GAlt g0 g1
    -> D.altPrinter (toPrinter g0) (toPrinter g1)

  GIsoMap iso ga
    -> D.isoMapPrinter iso (toPrinter ga)

  GProductMap ga gb
    -> D.productMapPrinter (toPrinter ga) (toPrinter gb)

-- Takes () on discarded result unlike Applicative
seqR :: Grammar () -> Grammar a -> Grammar a
seqR g0 g1 = inverseIso unitI . flipI \$/ g0 \*/ g1
(*/) = seqR

seqL :: Grammar a -> Grammar () -> Grammar a
seqL g0 g1 = inverseIso unitI \$/ g0 \*/ g1
(\*) = seqL

-- | A string of Text
textIs :: Text -> Grammar ()
textIs txt = case T.uncons txt of
  Nothing
    -> GPure ()

  Just (c,cs)
    ->  inverseIso (elementIso ((), ()))
    \$/ (inverseIso (elementIso c) \$/ anyChar)
    \*/ (textIs cs)

-- | A single character.
charIs :: Char -> Grammar ()
charIs = textIs . T.singleton

-- | Any single character
anyChar :: Grammar Char
anyChar = GAnyChar

-- | A character that matches a predicate
charWhen :: (Char -> Bool) -> Grammar Char
charWhen p = predI \$/ anyChar
  where
    predI = Iso
      (\c -> if p c then Just c else Nothing)
      (\c -> if p c then Just c else Nothing)

upper :: Grammar Char
upper = charWhen isUpper

lower :: Grammar Char
lower = charWhen isLower

digit :: Grammar Char
digit = charWhen isDigit

arrow      = charIs '→' \|/ textIs "->"
bar        = charIs '|'
star       = charIs '*'
plus       = charIs '+'
comma      = charIs ','
upArrow    = charIs '^'
lambda     = charIs 'λ' \|/ charIs '\\'
langle     = charIs '<'
rangle     = charIs '>'
lparen     = charIs '('
rparen     = charIs ')'
underscore = charIs '_'
union      = charIs '∪'
question   = charIs '?'
at         = charIs '@'
bigLambda  = charIs 'Λ' \|/ textIs "/\\"
bigAt      = charIs '#'

-- | A Grammar between two others.
between :: Grammar () -> Grammar a -> Grammar () -> Grammar a
between l a r = l */ a \* r

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
natural = naturalI \$/ longestMatching1 (P.Predicate isDigit $ P.ExpectPredicate "ISNATURAL")
  where
    naturalI :: Iso Text Int
    naturalI = Iso
      (Just . read . T.unpack) -- TODO: partial
      (Just . T.pack . show)

-- | A list of alternative Grammars.
alternatives :: [Grammar a] -> Grammar a
alternatives = foldr GAlt GEmpty

grammarMany :: Grammar a -> Grammar [a]
grammarMany g
  = consI \$/ g \*/ grammarMany g
 \|/ nilI \$/ GPure ()

grammarMany1 :: Grammar a -> Grammar [a]
grammarMany1 g = consI \$/ g \*/ grammarMany1 g

infixl 3 \|/
infixr 6 \*/
infix 5 \$/

(\|/) = GAlt
(\*/) = GProductMap
(\$/) = GIsoMap

