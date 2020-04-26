{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
  , StandaloneDeriving
  , LambdaCase
  , RankNTypes
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
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE

data Error phase

  -- | Generic error
  = EMsg Doc

  -- No such name
  | ETypeNotDefined TypeName -- ^ No such type
  | ETermNotDefined TermName -- ^ No such term

  -- | Two typed things cannot be applied to each other
  | EAppMismatch (TypeFor phase) (TypeFor phase) --

  -- | Something with type cannot be big-applied to something with kind
  | EBigAppMismatch (TypeFor phase) Kind

  -- | Something with kind cannot be type-applied to something with kind
  | ETypeAppMismatch !Kind !Kind

  -- | A Type app must apply a type lambda.
  | ETypeAppLambda (TypeFor phase)

  -- | An expression had a type, and claimed to have the type indexed within a
  -- sum type but doesnt.
  | ESumMismatch (TypeFor phase) Int (NonEmpty (TypeFor phase))

  -- | The default branch and the first branch of a case statement have
  -- different types.
  | ECaseDefaultMismatch (TypeFor phase) (TypeFor phase)

deriving instance
  (Eq (NamedExtension phase)
  ,Eq (ArrowExtension phase)
  ,Eq (SumTExtension phase)
  ,Eq (ProductTExtension phase)
  ,Eq (UnionTExtension phase)
  ,Eq (BigArrowExtension phase)
  ,Eq (TypeLamExtension phase)
  ,Eq (TypeAppExtension phase)
  ,Eq (TypeBindingExtension phase)
  ,Eq (TypeExtension phase)
  ,Eq (TypeBindingFor phase)
  )
  => Eq (Error phase)

deriving instance
  (Ord (NamedExtension phase)
  ,Ord (ArrowExtension phase)
  ,Ord (SumTExtension phase)
  ,Ord (ProductTExtension phase)
  ,Ord (UnionTExtension phase)
  ,Ord (BigArrowExtension phase)
  ,Ord (TypeLamExtension phase)
  ,Ord (TypeAppExtension phase)
  ,Ord (TypeBindingExtension phase)
  ,Ord (TypeExtension phase)
  ,Ord (TypeBindingFor phase)
  )
  => Ord (Error phase)

deriving instance
  (Show (NamedExtension phase)
  ,Show (ArrowExtension phase)
  ,Show (SumTExtension phase)
  ,Show (ProductTExtension phase)
  ,Show (UnionTExtension phase)
  ,Show (BigArrowExtension phase)
  ,Show (TypeLamExtension phase)
  ,Show (TypeAppExtension phase)
  ,Show (TypeBindingExtension phase)
  ,Show (TypeExtension phase)
  ,Show (TypeBindingFor phase)
  )
  => Show (Error phase)


ppError :: (TypeFor phase -> Doc) -> Error phase -> Doc
ppError ppType = \case
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

instance (Document (TypeFor phase)) => Document (Error phase) where
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
                 , mconcat . NE.toList . fmap document $ sumTys
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

