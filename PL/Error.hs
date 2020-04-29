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

data Error typ pattern

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

  -- | A case is matching on an expression with one type but the Pattern
  -- pattern has another.
  | EPatternMismatch typ pattern

deriving instance
  (Eq typ
  ,Eq pattern
  )
  => Eq (Error typ pattern)

deriving instance
  (Ord typ
  ,Ord pattern
  )
  => Ord (Error typ pattern)

deriving instance
  (Show typ
  ,Show pattern
  )
  => Show (Error typ pattern)


-- | We can pretty-print an error provided we're told how to pretty print
-- contained:
-- - Patterns
-- - Types
ppError
  :: (pattern -> Doc)
  -> (typ -> Doc)
  -> Error typ pattern -> Doc
ppError ppPattern ppType = \case
  EMsg doc
    -> doc

  ETypeNotDefined name context
    -> mconcat [ text "Type named:"
               , lineBreak
               , indent1 $ document name
               , lineBreak
               , text "is not defined in the context: "
               , lineBreak
               , indent1 $ text context
               ]

  ETermNotDefined name
    -> mconcat [ text "Term named:"
               , lineBreak
               , indent1 $ document name
               , lineBreak
               , text "is not defined."
               ]

  EAppMismatch fTy xTy
    -> mconcat [ text "Cannot apply expression typed:"
               , lineBreak
               , indent1 $ ppType fTy
               , lineBreak
               , text "to expression typed: "
               , lineBreak
               , indent1 $ ppType xTy
               ]

  EBigAppMismatch fTy xKy
    -> mconcat [ text "Cannot big-apply expression typed: "
               , lineBreak
               , indent1 $ ppType fTy
               , lineBreak
               , indent1 $ text "to type kinded:"
               , lineBreak
               , indent1 $ document xKy
               ]

  ETypeAppMismatch fKy xKy
    -> mconcat [ text "Cannot type-apply type kinded:"
               , lineBreak
               , indent1 $ document fKy
               , lineBreak
               , text "to type kinded: "
               , lineBreak
               , indent1 $ document xKy
               ]

  ETypeAppLambda fTy
    -> mconcat [ text "Cannot type-apply a non type-lam: "
               , lineBreak
               , indent1 $ ppType fTy
               ]

  ESumMismatch actualType index sumTys
    -> mconcat [ text "Expression had type: "
               , lineBreak
               , indent1 $ ppType actualType
               , lineBreak
               , text "and claimed to be contained within the sum:"
               , lineBreak
               , indent1 $ mconcat . NE.toList . fmap ppType $ sumTys
               , lineBreak
               , text "at index:"
               , lineBreak
               , indent1 $ document index
               ]

  ECaseDefaultMismatch defaultTy firstBranchTy
    -> mconcat [ text "In a case statement the default branch had type: "
               , lineBreak
               , indent1 $ ppType defaultTy
               , lineBreak
               , text "Whereas the first branch had type: "
               , lineBreak
               , indent1 $ ppType firstBranchTy
               , lineBreak
               , text "But branches must have the same type."
               ]

  EPatternMismatch expectedTy gotPattern
    -> mconcat [ text "In a case analysis the scrutinee expression had type: "
               , lineBreak
               , indent1 $ ppType expectedTy
               , lineBreak
               , text "but this type is not matched by a given pattern: "
               , lineBreak
               , indent1 $ ppPattern gotPattern
               ]

  ETypeReductionLimitReached typ
    -> mconcat [ text "Aborted reducing a type due to hitting the provided reduction limit. Aborted with the type: "
               , lineBreak
               , indent1 $ ppType typ
               ]

instance (Document typ, Document pattern) => Document (Error typ pattern) where
  document e = text "ERROR: " <> case e of
    EMsg doc
      -> doc

    ETypeNotDefined name context
      -> mconcat [ text "Type named:"
                 , lineBreak
                 , indent1 $ document name
                 , lineBreak
                 , text "is not defined in the context: "
                 , lineBreak
                 , indent1 $ text context
                 ]

    ETermNotDefined name
      -> mconcat [ text "Term named:"
                 , lineBreak
                 , indent1 $ document name
                 , lineBreak
                 , text "is not defined."
                 ]

    EAppMismatch fTy xTy
      -> mconcat [ text "Cannot apply expression typed:"
                 , lineBreak
                 , indent1 $ document fTy
                 , lineBreak
                 , text "to expression typed:"
                 , lineBreak
                 , indent1 $ document xTy
                 ]

    EBigAppMismatch fTy xKy
      -> mconcat [ text "Cannot big-apply expression typed:"
                 , lineBreak
                 , indent1 $ document fTy
                 , lineBreak
                 , text "to type kinded:"
                 , lineBreak
                 , indent1 $ document xKy
                 ]

    ETypeAppMismatch fKy xKy
      -> mconcat [ text "Cannot type-apply type kinded"
                 , lineBreak
                 , indent1 $ document fKy
                 , lineBreak
                 , text "to type kinded:"
                 , lineBreak
                 , indent1 $ document xKy
                 ]

    ETypeAppLambda fTy
      -> mconcat [ text "Cannot type-apply a non type-lam: "
                 , lineBreak
                 , indent1 $ document fTy
                 ]

    ESumMismatch actualType index sumTys
      -> mconcat [ text "Expression had type: "
                 , lineBreak
                 , indent1 $ document actualType
                 , lineBreak
                 , text "and claimed to be contained within the sum:"
                 , indent1 $ mconcat . NE.toList . fmap document $ sumTys
                 , lineBreak
                 , text "at index:"
                 , lineBreak
                 , indent1 $ document index
                 ]

    ECaseDefaultMismatch defaultTy firstBranchTy
      -> mconcat [ text "In a case statement the default branch had type: "
                 , lineBreak
                 , indent1 $ document defaultTy
                 , lineBreak
                 , indent1 $ text "whereas the first branch had type: "
                 , lineBreak
                 , indent1 $ document firstBranchTy
                 , lineBreak
                 , text "but branches must have the same type."
                 ]

    EPatternMismatch expectedTy gotPattern
      -> mconcat [ text "in a case analysis the scrutinee expression had type: "
                 , lineBreak
                 , indent1 $ document expectedTy
                 , lineBreak
                 , text "but this type is not matched by a given pattern: "
                 , lineBreak
                 , indent1 $ document gotPattern
                 ]

    ETypeReductionLimitReached typ
      -> mconcat [ text "Aborted reducing a type due to hitting the provided reduction limit. Aborted with the type: "
                 , lineBreak
                 , indent1 $ document typ
                 ]

