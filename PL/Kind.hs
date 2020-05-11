{-# LANGUAGE
    OverloadedStrings
  #-}
{-|
Module      : PL.Kind
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

The Type of Types.
-}
module PL.Kind
  ( Kind (..)
  , kindEq
  )
  where

import PL.Hash

import PLPrinter
import PLPrinter.Doc

import Data.Monoid

-- Describe properties of types
data Kind

  -- | Simple kind
  = Kind

  -- | Kind of type lambdas
  | KindArrow
    {_from :: Kind
    ,_to   :: Kind
    }
  deriving (Eq,Ord,Show)

kindEq :: Kind -> Kind -> Bool
kindEq = (==)

instance Document Kind where
  document k = case k of
    Kind
      -> text "KIND"

    KindArrow from to
      -> char '^' <> parens (document from) <> parens (document to)

instance Hashable Kind where
  toHashToken k = case k of
    Kind
      -> HashTag "Kind" []

    KindArrow fromKy toKy
      -> HashTag "KindArrow" [toHashToken fromKy, toHashToken toKy]

