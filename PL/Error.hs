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
import PL.Bindings
import PL.Binds
import PL.Var
import PL.TyVar

import PLPrinter

import Data.Monoid
import Data.Text
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE

data Error expr typ pattern

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

  -- | A Union pattern should only match a single typ.
  | EMultipleMatchesInUnion [typ]

  -- | Failed to lookup an expression binding ix in some context.
  | EBindExprLookupFailure Int (Bindings expr)

  -- | Failed to lookup a type binding ix in some context.
  | EBindTypeLookupFailure Int (Bindings typ)

  -- | Failed to lookup an expressions type
  | EBindCtxExprLookupFailure Int (BindCtx Var typ)

  -- | Failed to lookup a types kind
  | EBindCtxTypeLookupFailure Int (BindCtx TyVar Kind)

  -- | An error is a context for some deeper error.
  | EContext (Error expr typ pattern) (Error expr typ pattern)

deriving instance
  (Eq expr
  ,Eq (Bindings expr)
  ,Eq typ
  ,Eq pattern
  )
  => Eq (Error expr typ pattern)

deriving instance
  (Eq (Bindings expr)
  ,Ord (Bindings expr)
  ,Ord expr
  ,Ord typ
  ,Ord pattern
  )
  => Ord (Error expr typ pattern)

deriving instance
  (Show expr
  ,Show typ
  ,Show pattern
  )
  => Show (Error expr typ pattern)


-- | We can pretty-print an error provided we're told how to pretty print
-- contained:
-- - Patterns
-- - Types
-- - Expressions
ppError
  :: (pattern -> Doc)
  -> (typ -> Doc)
  -> (expr -> Doc)
  -> (Var -> Doc)
  -> (TyVar -> Doc)
  -> Error expr typ pattern -> Doc
ppError ppPattern ppType ppExpr ppVar ppTyVar = \case
  -- TODO: Wack parameter order

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

  EMultipleMatchesInUnion typs
    -> mconcat [ text "Exactly one match is expected, but matched types:"
               , lineBreak
               , indent1 $ mconcat $ fmap ppType typs
               ]

  EBindExprLookupFailure ix bindings
    -> mconcat [ text "Failed to lookup an expression binding with index:"
               , lineBreak
               , indent1 $ int ix
               , lineBreak
               , text "In bindings:"
               , lineBreak
               , ppBindingsTree ppExpr bindings
               ]

  EBindTypeLookupFailure ix bindings
    -> mconcat [ text "Failed to lookup a type binding with index:"
               , lineBreak
               , indent1 $ int ix
               , lineBreak
               , text "In bindings:"
               , lineBreak
               , ppBindingsTree ppType bindings
               ]

  EBindCtxExprLookupFailure ix bindCtx
    -> mconcat [ text "Failed to lookup an expressions type from a binding with index:"
               , lineBreak
               , indent1 $ int ix
               , lineBreak
               , text "In bind ctx:"
               , lineBreak
               , ppBindCtx ppVar ppType bindCtx
               ]

  EBindCtxTypeLookupFailure ix bindCtx
    -> mconcat [ text "Failed to lookup a types kind from a binding with index:"
               , lineBreak
               , indent1 $ int ix
               , lineBreak
               , text "In bind ctx:"
               , lineBreak
               , ppBindCtx ppTyVar document bindCtx
               ]

  EContext context err
    -> mconcat [ text "Error:"
               , lineBreak
               , indent1 $ ppError ppPattern ppType ppExpr ppVar ppTyVar err
               , lineBreak
               , text "In context:"
               , lineBreak
               , indent1 $ ppError ppPattern ppType ppExpr ppVar ppTyVar context
               ]

instance (Document expr, Document typ, Document pattern) => Document (Error expr typ pattern) where
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

    EMultipleMatchesInUnion typs
      -> mconcat [ text "Exactly one match is expected, but matched types:"
                 , lineBreak
                 , indent1 $ mconcat $ fmap document typs
                 ]

    EBindExprLookupFailure ix bindings
      -> mconcat [ text "Failed to lookup an expression binding with index:"
                 , lineBreak
                 , indent1 $ int ix
                 , lineBreak
                 , text "In bindings:"
                 , lineBreak
                 , document bindings
                 ]

    EBindTypeLookupFailure ix bindings
      -> mconcat [ text "Failed to lookup a type binding with index:"
                 , lineBreak
                 , indent1 $ int ix
                 , lineBreak
                 , text "In bindings:"
                 , lineBreak
                 , document bindings
                 ]

    EBindCtxExprLookupFailure ix bindCtx
      -> mconcat [ text "Failed to lookup an expressions type from a binding with index:"
                 , lineBreak
                 , indent1 $ int ix
                 , lineBreak
                 , text "In bind ctx:"
                 , lineBreak
                 , document bindCtx
                 ]

    EBindCtxTypeLookupFailure ix bindCtx
      -> mconcat [ text "Failed to lookup a types kind from a binding with index:"
                 , lineBreak
                 , indent1 $ int ix
                 , lineBreak
                 , text "In bind ctx:"
                 , lineBreak
                 , document bindCtx
                 ]


    EContext context err
      -> mconcat [ text "Error: "
                 , lineBreak
                 , indent1 $ document err
                 , lineBreak

                 , text "In context:"
                 , lineBreak
                 , indent1 $ document context
                 ]

