{-# LANGUAGE
    OverloadedStrings
  , TypeFamilies
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

-- PL
import PL.Error
import PL.FixPhase

-- External PL
import PLPrinter
import PLPrinter.Doc
import PLHash

-- Other

-- Describe properties of types
data Kind

  -- | Simple kind
  = Kind

  -- | Kind of type lambdas
  | KindArrow
    {_fromKind :: Kind
    ,_toKind   :: Kind
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

type instance ErrorKind DefaultPhase = Kind
