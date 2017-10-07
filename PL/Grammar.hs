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
-}
module PL.Grammar
  ( Grammar()
  , charIs
  , textIs
  )
  where

import Data.Text (Text)
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

