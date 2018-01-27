{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Error
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Errors that may be throws in various compilation stages.
-}

module PL.Error where

import PL.Kind
import PL.Name
import PL.PLPrinter.Printer
import PL.Type

import Data.Monoid
import Data.Text

data Error tb

  -- ^ Generic error
  = EMsg Text

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
  document e = DocText "ERROR: " <> case e of
    EMsg msg
      -> DocText msg

    ETypeNotDefined name
      -> mconcat [DocText "Type named '"
                 ,document name
                 ,DocText "' is not defined."
                 ]

    ETermNotDefined name
      -> mconcat [DocText "Term named '"
                 ,document name
                 ,DocText "' is not defined."
                 ]

    EAppMismatch fTy xTy
      -> mconcat [DocText "Cannot apply expression typed: '"
                 ,document fTy
                 ,DocText "' to expression typed: '"
                 ,document xTy
                 ,DocText "'."
                 ]

    EBigAppMismatch fTy xKy
      -> mconcat [DocText "Cannot big-apply expression typed: '"
                 ,document fTy
                 ,DocText "' to type kinded: '"
                 ,document xKy
                 ,DocText "'."
                 ]

    ETypeAppMismatch fKy xKy
      -> mconcat [DocText "Cannot type-apply type kinded: '"
                 ,document fKy
                 ,DocText "' to type kinded: '"
                 ,document xKy
                 ,DocText "'."
                 ]

