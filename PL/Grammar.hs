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

import PL.Iso
import Prelude hiding ((.),id)
import Control.Category

-- | The grammar of some language.
data Grammar a where
  -- Any character
  GAnyChar
    :: Grammar Char

  -- Applicative-like pure
  GPure
    :: Eq a
    => a
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
    :: Show a
    => Iso a b
    -> Grammar a
    -> Grammar b

  GProductMap
    :: Show a
    => Grammar a
    -> Grammar b
    -> Grammar (a,b)

-- Takes () on discarded result unlike Applicative
(*/), seqR :: Show a => Grammar () -> Grammar a -> Grammar a
(*/) = seqR
seqR g0 g1 = inverseIso unitI . flipI \$/ g0 \*/ g1


(\*), seqL :: Show a => Grammar a -> Grammar () -> Grammar a
(\*) = seqL
seqL g0 g1 = inverseIso unitI \$/ g0 \*/ g1

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
between :: Show a => Grammar () -> Grammar a -> Grammar () -> Grammar a
between l a r = l */ a \* r

-- | A Grammar between parentheses.
betweenParens :: Show a => Grammar a -> Grammar a
betweenParens a = between lparen a rparen

-- | Longest matching text.
longestMatching :: (Char -> Bool) -> Grammar Text
longestMatching p = concatI \$/ grammarMany (charWhen p)
  where
    concatI :: Iso String Text
    concatI = Iso
      (Just . T.pack)
      (Just . T.unpack)

-- | Longest matching text. At least one character.
longestMatching1 :: (Char -> Bool) -> Grammar Text
longestMatching1 p = concatI \$/ grammarMany1 (charWhen p)
  where
    concatI :: Iso String Text
    concatI = Iso
      (Just . T.pack)
      (Just . T.unpack)

-- | A natural number: zero and positive integers
natural :: Grammar Int
natural = naturalI \$/ longestMatching1 isDigit
  where
    naturalI :: Iso Text Int
    naturalI = Iso
      (Just . read . T.unpack) -- TODO: partial
      (Just . T.pack . show)

-- | A list of alternative Grammars.
alternatives :: [Grammar a] -> Grammar a
alternatives = foldr GAlt GEmpty

grammarMany :: (Eq a,Show a) => Grammar a -> Grammar [a]
grammarMany g = grammarMany1 g \|/ GPure []

grammarMany1 :: (Eq a,Show a) => Grammar a -> Grammar [a]
grammarMany1 g = consI \$/ g \*/ grammarMany g

infixl 3 \|/
infixr 6 \*/
infix 5 \$/

(\|/) = GAlt

(\*/) :: Show a => Grammar a -> Grammar b -> Grammar (a,b)
(\*/) = GProductMap

(\$/) :: Show a => Iso a b -> Grammar a -> Grammar b
(\$/) = GIsoMap

