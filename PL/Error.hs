{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Error
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Errors that may be throws in various compilation stages.
-}

module PL.Error where

import PL.Name
import PL.Type
import PL.Kind

import PL.Printer

import Data.Monoid

data Error tb

  -- ^ Generic error
  = EMsg String

  -- No such name
  | ETypeNotDefined TypeName -- ^ No such type
  | ETermNotDefined TermName -- ^ No such term

  -- ^ Two typed things cannot be applied to each other
  | EAppMismatch (Type tb) (Type tb) --

  -- ^ Something with type cannot be big-applied to something with kind
  | EBigAppMismatch (Type tb) Kind

  -- ^ Something with kind cannot be type-applied to something with kind
  | ETypeAppMismatch Kind Kind
  deriving (Ord,Eq,Show)

instance Document tb => Document (Error tb) where
  document e = "ERROR: " <> case e of
    EMsg msg
      -> string msg

    ETypeNotDefined name
      -> "Type named '" <> document name <> "' is not defined."

    ETermNotDefined name
      -> "Term named '" <> document name <> "' is not defined."

    EAppMismatch fTy xTy
      -> "Cannot apply expression typed: '" <> document fTy <> "' to expression typed: '" <> document xTy <> "'."

    EBigAppMismatch fTy xKy
      -> "Cannot big-apply expression typed: '" <> document fTy <> "' to type kinded: '" <> document xKy <> "'."

    ETypeAppMismatch fKy xKy
      -> "Cannot type-apply type kinded: '" <> document fKy <> "' to type kinded: '" <> document xKy <> "'."

