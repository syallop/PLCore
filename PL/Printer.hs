{-# LANGUAGE
    FlexibleInstances
  , OverloadedStrings
  , GADTs
  , RankNTypes
  , UndecidableInstances
  #-}
{-|
Module      : PL.Printer
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A NIH Pretty-Printer
-}
module PL.Printer
  (-- * Types
    Doc(..)
  , DocFmt()

   -- * Render a Doc
  , mkDocFmt
  , docFmt
  , render
  , renderWith

   -- * Create Docs
   -- ** From basic text
  , char
  , text
  , string

  , usingShow

   -- ** Indentation
  , indent
  , indent1

   -- ** From primitive types
  , int
  , bool

   -- **
  , between
  , emptyDoc

  , lineBreak
  , newLine

   -- * Class of things which have a canonical Doc
  , Document
  , document
  , renderDocument

  , Printer
  , isoMapPrinter
  , productMapPrinter
  , emptyPrinter
  , altPrinter
  , purePrinter
  , anyCharPrinter
  , toPrinter
  , pprint
  )
  where

import Data.List
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad
import Control.Applicative

import PL.PLGrammar.Iso
import PL.PLGrammar.Grammar hiding (between)

import PL.Printer.Doc

newtype Printer a = Printer {_unPPrint :: a -> Maybe Doc}

pprint :: Printer a -> a -> Maybe Doc
pprint = _unPPrint

isoMapPrinter :: Iso a b -> Printer a -> Printer b
isoMapPrinter iso (Printer p) = Printer $ printIso iso >=> p

productMapPrinter :: Printer a -> Printer b -> Printer (a,b)
productMapPrinter (Printer p) (Printer q) = Printer $ \(a,b) -> liftM2 mappend (p a) (q b)

-- These functions cant form an Alternative because we can't implement
-- the superclass Functor as we're a contravariant functor.
emptyPrinter :: Printer a
emptyPrinter = Printer . const $ Nothing

altPrinter :: Printer a -> Printer a -> Printer a
altPrinter (Printer p) (Printer q) = Printer $ \a -> mplus (p a) (q a)

purePrinter :: Eq a => a -> Printer a
purePrinter a = Printer $ \a' -> if a == a' then Just DocEmpty else Nothing

anyCharPrinter :: Printer Char
anyCharPrinter = Printer $ Just . char

anyTextPrinter :: Printer Text
anyTextPrinter = Printer $ Just . text

toPrinter :: Grammar a -> Printer a
toPrinter grammar = case grammar of
  GAnyChar
    -> anyCharPrinter

  GAnyText
    -> anyTextPrinter

  GPure a
    -> purePrinter a

  GEmpty
    -> emptyPrinter

  GAlt g0 g1
    -> altPrinter (toPrinter g0) (toPrinter g1)

  GIsoMap iso ga
    -> isoMapPrinter iso (toPrinter ga)

  GProductMap ga gb
    -> productMapPrinter (toPrinter ga) (toPrinter gb)

  GLabel _label g
    -> toPrinter g

