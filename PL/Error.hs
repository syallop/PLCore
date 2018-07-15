{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
  #-}
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
import PL.Type

import PLPrinter

import Data.Monoid
import Data.Text

data Error tb

  -- ^ Generic error
  = EMsg Doc

  -- No such name
  | ETypeNotDefined TypeName -- ^ No such type
  | ETermNotDefined TermName -- ^ No such term

  -- ^ Two typed things cannot be applied to each other
  | EAppMismatch (Type tb) (Type tb) --

  -- ^ Something with type cannot be big-applied to something with kind
  | EBigAppMismatch (Type tb) Kind

  -- ^ Something with kind cannot be type-applied to something with kind
  | ETypeAppMismatch !Kind !Kind

  -- ^ A Type app must apply a type lambda.
  | ETypeAppLambda (Type tb)

  -- ^ An expression had a type, and claimed to have the type indexed within a
  -- sum type but doesnt.
  | ESumMismatch (Type tb) Int [Type tb]

  -- ^ The default branch and the first branch of a case statement have
  -- different types.
  | ECaseDefaultMismatch (Type tb) (Type tb)
  deriving (Ord,Eq,Show)

instance (Document (Type tb)) => Document (Error tb) where
  document e = text "ERROR: " <> case e of
    EMsg doc
      -> doc

    ETypeNotDefined name
      -> mconcat [ text "Type named '"
                 , document name
                 , text "' is not defined."
                 ]

    ETermNotDefined name
      -> mconcat [ text "Term named '"
                 , document name
                 , text "' is not defined."
                 ]

    EAppMismatch fTy xTy
      -> mconcat [ text "Cannot apply expression typed: '"
                 , document fTy
                 , text "' to expression typed: '"
                 , document xTy
                 , text "'."
                 ]

    EBigAppMismatch fTy xKy
      -> mconcat [ text "Cannot big-apply expression typed: '"
                 , document fTy
                 , text "' to type kinded: '"
                 , document xKy
                 , text "'."
                 ]

    ETypeAppMismatch fKy xKy
      -> mconcat [ text "Cannot type-apply type kinded: '"
                 , document fKy
                 , text "' to type kinded: '"
                 , document xKy
                 , text "'."
                 ]

    ETypeAppLambda fTy
      -> mconcat [ text "Cannot type-apply a non type-lam: "
                 , document fTy
                 ]

    ESumMismatch actualType index sumTys
      -> mconcat [ text "Expression had type: "
                 , document actualType
                 , text "and claimed to be contained within the sum"
                 , mconcat . fmap document $ sumTys
                 , text "at index"
                 , document index
                 ]

    ECaseDefaultMismatch defaultTy firstBranchTy
      -> mconcat [ text "In a case statement the default branch had type: "
                 , document defaultTy
                 , text "whereas the first branch had type: "
                 , document firstBranchTy
                 , text " but branches must have the same type."
                 ]
