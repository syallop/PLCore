{-# LANGUAGE OverloadedStrings
           , OverlappingInstances
  #-}
{-|
Module      : PL.Kind
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

The Type of Types.
-}
module PL.Kind where

import PL.Printer
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

