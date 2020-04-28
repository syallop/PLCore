{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
  , StandaloneDeriving
  , LambdaCase
  , RankNTypes
  , KindSignatures
  , PolyKinds
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

import PLPrinter

import Data.Monoid
import Data.Text
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE

data Error typ matchArg

  -- | Generic error
  = EMsg Doc

  -- No such name in some context
  | ETypeNotDefined TypeName Text -- ^ No such type
  | ETermNotDefined TermName -- ^ No such term

  -- | Two typed things cannot be applied to each other
  | EAppMismatch typ typ --

  -- | Something with type cannot be big-applied to something with kind
  | EBigAppMismatch typ Kind

  -- | Something with kind cannot be type-applied to something with kind
  | ETypeAppMismatch !Kind !Kind

  -- | A Type app must apply a type lambda.
  | ETypeAppLambda typ

  -- | An expression had a type, and claimed to have the type indexed within a
  -- sum type but doesnt.
  | ESumMismatch typ Int (NonEmpty typ)

  -- | The default branch and the first branch of a case statement have
  -- different types.
  | ECaseDefaultMismatch typ typ

  -- | A given reduction limit has been exceeded when trying to reduce a type.
  | ETypeReductionLimitReached typ

  -- | A case is matching on an expression with one type but the MatchArg
  -- pattern has another.
  | EPatternMismatch typ matchArg

deriving instance
  (Eq typ
  ,Eq matchArg
  )
  => Eq (Error typ matchArg)

deriving instance
  (Ord typ
  ,Ord matchArg
  )
  => Ord (Error typ matchArg)

deriving instance
  (Show typ
  ,Show matchArg
  )
  => Show (Error typ matchArg)


-- | We can pretty-print an error provided we're told how to pretty print
-- contained:
-- - MatchArgs
-- - Types
ppError
  :: (matchArg -> Doc)
  -> (typ -> Doc)
  -> Error typ matchArg -> Doc
ppError ppMatchArg ppType = \case
  EMsg doc
    -> doc

  ETypeNotDefined name context
    -> mconcat [ text "Type named '"
               , document name
               , text "' is not defined in the context: "
               , text context
               ]

  ETermNotDefined name
    -> mconcat [ text "Term named '"
               , document name
               , text "' is not defined."
               ]

  EAppMismatch fTy xTy
    -> mconcat [ text "Cannot apply expression typed: '"
               , ppType fTy
               , text "' to expression typed: '"
               , ppType xTy
               , text "'."
               ]

  EBigAppMismatch fTy xKy
    -> mconcat [ text "Cannot big-apply expression typed: '"
               , ppType fTy
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
               , ppType fTy
               ]

  ESumMismatch actualType index sumTys
    -> mconcat [ text "Expression had type: "
               , ppType actualType
               , text "and claimed to be contained within the sum"
               , mconcat . NE.toList . fmap ppType $ sumTys
               , text "at index"
               , document index
               ]

  ECaseDefaultMismatch defaultTy firstBranchTy
    -> mconcat [ text "In a case statement the default branch had type: "
               , ppType defaultTy
               , text "whereas the first branch had type: "
               , ppType firstBranchTy
               , text " but branches must have the same type."
               ]

  EPatternMismatch expectedTy gotPattern
    -> mconcat [ text "in a case analysis the scrutinee expression had type: "
               , ppType expectedTy
               , text " but this type is not matched by a given pattern: "
               , ppMatchArg gotPattern
               ]

  ETypeReductionLimitReached typ
    -> mconcat [ text "Aborted reducing a type due to hitting the provided reduction limit. Aborted with the type: "
               , lineBreak
               , ppType typ
               ]

instance (Document typ, Document matchArg) => Document (Error typ matchArg) where
  document e = text "ERROR: " <> case e of
    EMsg doc
      -> doc

    ETypeNotDefined name context
      -> mconcat [ text "Type named '"
                 , document name
                 , text "' is not defined in the context: "
                 , text context
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
                 , mconcat . NE.toList . fmap document $ sumTys
                 , text "at index"
                 , document index
                 ]

    ECaseDefaultMismatch defaultTy firstBranchTy
      -> mconcat [ text "In a case statement the default branch had type: "
                 , document defaultTy
                 , text " whereas the first branch had type: "
                 , document firstBranchTy
                 , text " but branches must have the same type."
                 ]

    EPatternMismatch expectedTy gotPattern
      -> mconcat [ text "in a case analysis the scrutinee expression had type: "
                 , document expectedTy
                 , text " but this type is not matched by a given pattern: "
                 , document gotPattern
                 ]

    ETypeReductionLimitReached typ
      -> mconcat [ text "Aborted reducing a type due to hitting the provided reduction limit. Aborted with the type: "
                 , lineBreak
                 , document typ
                 ]

