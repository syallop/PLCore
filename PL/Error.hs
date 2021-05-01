{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
  , StandaloneDeriving
  , LambdaCase
  , RankNTypes
  , KindSignatures
  , PolyKinds
  , TypeFamilies
  #-}
{-|
Module      : PL.Error
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Errors that may be throws in various compilation stages.
-}

module PL.Error
  ( ErrorFor
      ( EMsg
      , EContext

      , ETypeChecking
      , ETypeNotDefined
      , EAppMismatch
      , EBigAppMismatch
      , ETypeAppMismatch
      , ETypeAppLambda
      , ESumMismatch
      , ECaseDefaultMismatch
      , ECaseBranchMismatch
      , ETypeReductionLimitReached
      , EPatternMismatch
      , EMultipleMatchesInUnion
      , EBindExprLookupFailure
      , EBindTypeLookupFailure
      , EBindCtxExprLookupFailure
      , EBindCtxTypeLookupFailure
      )

  , ErrorExpr
  , ErrorType
  , ErrorPattern
  , ErrorKind
  , ErrorTypeCtx
  , ErrorTypeName
  , ErrorBinding
  , ErrorTypeBinding

  , PPError (..)
  , ppError

  , liftEMsg
  )
  where

import PL.Bindings
import PL.Binds

import PLPrinter

import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE

type family ErrorExpr phase
type family ErrorType phase
type family ErrorPattern phase
type family ErrorKind phase
type family ErrorTypeCtx phase
type family ErrorTypeName phase
type family ErrorBinding phase
type family ErrorTypeBinding phase

data ErrorFor phase

  -- | Generic error
  = EMsg Doc

  -- | An error is a context for some deeper error.
  | EContext (ErrorFor phase) (ErrorFor phase)

  -- | An error occured type checking an expression.
  | ETypeChecking (ErrorExpr phase)

  -- | No such type in context
  | ETypeNotDefined (ErrorTypeName phase) (ErrorTypeCtx phase)

  -- | Two typed things cannot be applied to each other
  | EAppMismatch (ErrorType phase) (ErrorType phase)

  -- | Something with type cannot be big-applied to something with kind
  | EBigAppMismatch (ErrorType phase) (ErrorKind phase)

  -- | Something with kind cannot be type-applied to something with kind
  | ETypeAppMismatch (ErrorKind phase) (ErrorKind phase)

  -- | A Type app must apply a type lambda.
  | ETypeAppLambda (ErrorType phase)

  -- | An expression had a type, and claimed to have the type indexed within a
  -- sum type but doesnt.
  | ESumMismatch (ErrorType phase) Int (NonEmpty (ErrorType phase))

  -- | The default branch and the first branch of a case statement have
  -- different types.
  | ECaseDefaultMismatch (ErrorType phase) (ErrorType phase)

  -- | We expected a case branch to have a type but it had another.
  | ECaseBranchMismatch (ErrorType phase) (ErrorType phase)

  -- | A given reduction limit has been exceeded when trying to reduce a type.
  | ETypeReductionLimitReached (ErrorType phase)

  -- | A case is matching on an expression with one type but the Pattern
  -- pattern has another.
  | EPatternMismatch (ErrorType phase) (ErrorPattern phase)

  -- | A Union pattern should only match a single typ.
  | EMultipleMatchesInUnion [ErrorType phase]

  -- | Failed to lookup an expression binding ix in some context.
  | EBindExprLookupFailure Int (Bindings (ErrorExpr phase))

  -- | Failed to lookup a type binding ix in some context.
  | EBindTypeLookupFailure Int (Bindings (ErrorType phase))

  -- | Failed to lookup an expressions type
  | EBindCtxExprLookupFailure Int (BindCtx (ErrorBinding phase) (ErrorType phase))

  -- | Failed to lookup a types kind
  | EBindCtxTypeLookupFailure Int (BindCtx (ErrorTypeBinding phase) (ErrorKind phase))


deriving instance
  ( Eq (ErrorExpr phase)
  , Eq (BindCtx (ErrorBinding phase) (ErrorType phase))
  , Eq (BindCtx (ErrorTypeBinding phase) (ErrorKind phase))
  , Eq (Bindings (ErrorExpr phase))
  , Eq (ErrorBinding phase)
  , Eq (ErrorKind phase)
  , Eq (ErrorPattern phase)
  , Eq (ErrorType phase)
  , Eq (ErrorTypeBinding phase)
  , Eq (ErrorTypeCtx phase)
  , Eq (ErrorTypeName phase)
  )
  => Eq (ErrorFor phase)

deriving instance
  ( Eq (ErrorBinding phase)
  , Eq (BindCtx (ErrorBinding phase) (ErrorType phase))
  , Eq (BindCtx (ErrorTypeBinding phase) (ErrorKind phase))
  , Eq (ErrorTypeBinding phase)
  , Eq (ErrorTypeName phase)
  , Ord (BindCtx (ErrorBinding phase) (ErrorType phase))
  , Ord (BindCtx (ErrorTypeBinding phase) (ErrorKind phase))
  , Ord (Bindings (ErrorExpr phase))
  , Ord (ErrorExpr phase)
  , Ord (ErrorKind phase)
  , Ord (ErrorPattern phase)
  , Ord (ErrorType phase)
  , Ord (ErrorTypeCtx phase)
  , Ord (ErrorTypeName phase)
  )
  => Ord (ErrorFor phase)

deriving instance
  ( Show (ErrorExpr phase)
  , Binds (ErrorBinding phase) (ErrorType phase)
  , Binds (ErrorTypeBinding phase) (ErrorKind phase)
  , Show (ErrorBinding phase)
  , Show (ErrorKind phase)
  , Show (ErrorPattern phase)
  , Show (ErrorType phase)
  , Show (ErrorTypeBinding phase)
  , Show (ErrorTypeCtx phase)
  , Show (ErrorTypeName phase)
  )
  => Show (ErrorFor phase)

data PPError phase = PPError
  { _ppExpr        :: ErrorExpr phase -> Doc
  , _ppType        :: ErrorType phase -> Doc
  , _ppPattern     :: ErrorPattern phase -> Doc
  , _ppKind        :: ErrorKind phase -> Doc
  , _ppTypeCtx     :: ErrorTypeCtx phase -> Doc
  , _ppTypeName    :: ErrorTypeName phase -> Doc
  , _ppBinding     :: ErrorBinding phase -> Doc
  , _ppTypeBinding :: ErrorTypeBinding phase -> Doc
  }

-- | We can pretty-print an error provided we're told how to pretty print
-- contained:
-- - Patterns
-- - Types
-- - Expressions
ppError
  :: ( Binds (ErrorBinding phase)     (ErrorType phase)
     , Binds (ErrorTypeBinding phase) (ErrorKind phase)
     )
  => PPError phase
  -> ErrorFor phase
  -> Doc
ppError pp = \case
  -- TODO: Wack parameter order

  EMsg doc
    -> doc

  EContext context err
    -> mconcat [ text "Error:"
               , lineBreak
               , indent1 . ppError pp $ err
               , lineBreak
               , text "In context:"
               , lineBreak
               , indent1 . ppError pp $ context
               ]

  ETypeChecking expr
    -> mconcat [ text "Checking type of expression:"
               , lineBreak
               , indent1 . _ppExpr pp $ expr
               ]


  ETypeNotDefined name typeCtx
    -> mconcat [ text "Type named:"
               , lineBreak
               , indent1 . _ppTypeName pp $ name
               , lineBreak
               , text "is not defined in the named type context: "
               , lineBreak
               , indent1 . _ppTypeCtx pp $ typeCtx
               ]

  EAppMismatch fTy xTy
    -> mconcat [ text "Cannot apply expression typed:"
               , lineBreak
               , indent1 . _ppType pp $ fTy
               , lineBreak
               , text "to expression typed: "
               , lineBreak
               , indent1 . _ppType pp $ xTy
               ]

  EBigAppMismatch fTy xKy
    -> mconcat [ text "Cannot big-apply expression typed: "
               , lineBreak
               , indent1 . _ppType pp $ fTy
               , lineBreak
               , indent1 $ text "to type kinded:"
               , lineBreak
               , indent1 . _ppKind pp $ xKy
               ]

  ETypeAppMismatch fKy xKy
    -> mconcat [ text "Cannot type-apply type kinded:"
               , lineBreak
               , indent1 . _ppKind pp $ fKy
               , lineBreak
               , text "to type kinded: "
               , lineBreak
               , indent1 . _ppKind pp $ xKy
               ]

  ETypeAppLambda fTy
    -> mconcat [ text "Cannot type-apply a non type-lam: "
               , lineBreak
               , indent1 . _ppType pp $ fTy
               ]

  ESumMismatch actualType sumIndex sumTys
    -> mconcat [ text "Expression had type: "
               , lineBreak
               , indent1 . _ppType pp $ actualType
               , lineBreak
               , text "and claimed to be contained within the sum:"
               , lineBreak
               , indent1 $ mconcat . NE.toList . fmap (_ppType pp) $ sumTys
               , lineBreak
               , text "at index:"
               , lineBreak
               , indent1 $ document sumIndex
               ]

  ECaseDefaultMismatch defaultTy firstBranchTy
    -> mconcat [ text "In a case statement the default branch had type: "
               , lineBreak
               , indent1 . _ppType pp $ defaultTy
               , lineBreak
               , text "Whereas the first branch had type: "
               , lineBreak
               , indent1 . _ppType pp $ firstBranchTy
               , lineBreak
               , text "But branches must have the same type."
               ]


  ECaseBranchMismatch expectedTy gotTy
    -> mconcat
        [ text "Case branch has unexpected type. Expected:"
        , lineBreak
        , indent1 . _ppType pp $ expectedTy
        , lineBreak
        , text "But got:"
        , lineBreak
        , indent1 . _ppType pp $ gotTy
        ]


  EPatternMismatch expectedTy gotPattern
    -> mconcat [ text "In a case analysis the scrutinee expression had type: "
               , lineBreak
               , indent1 . _ppType pp $ expectedTy
               , lineBreak
               , text "but this type is not matched by a given pattern: "
               , lineBreak
               , indent1 . _ppPattern pp $ gotPattern
               ]

  ETypeReductionLimitReached typ
    -> mconcat [ text "Aborted reducing a type due to hitting the provided reduction limit. Aborted with the type: "
               , lineBreak
               , indent1 . _ppType pp $ typ
               ]

  EMultipleMatchesInUnion typs
    -> mconcat [ text "Exactly one match is expected, but matched types:"
               , lineBreak
               , indent1 $ mconcat $ fmap (_ppType pp) typs
               ]

  EBindExprLookupFailure ix bindings
    -> mconcat [ text "Failed to lookup an expression binding with index:"
               , lineBreak
               , indent1 $ int ix
               , lineBreak
               , text "In bindings:"
               , lineBreak
               , ppBindingsTree (_ppExpr pp) bindings
               ]

  EBindTypeLookupFailure ix bindings
    -> mconcat [ text "Failed to lookup a type binding with index:"
               , lineBreak
               , indent1 $ int ix
               , lineBreak
               , text "In bindings:"
               , lineBreak
               , ppBindingsTree (_ppType pp) bindings
               ]

  EBindCtxExprLookupFailure ix bindCtx
    -> mconcat [ text "Failed to lookup an expressions type from a binding with index:"
               , lineBreak
               , indent1 $ int ix
               , lineBreak
               , text "In bind ctx:"
               , lineBreak
               , ppBindCtx (_ppBinding pp) (_ppType pp) bindCtx
               ]

  EBindCtxTypeLookupFailure ix bindCtx
    -> mconcat [ text "Failed to lookup a types kind from a binding with index:"
               , lineBreak
               , indent1 $ int ix
               , lineBreak
               , text "In bind ctx:"
               , lineBreak
               , ppBindCtx (_ppTypeBinding pp) (_ppKind pp) bindCtx
               ]


instance
  ( Document (ErrorExpr phase)
  , Document (ErrorType phase)
  , Document (ErrorKind phase)
  , Document (ErrorPattern phase)
  , Document (ErrorTypeCtx phase)
  , Document (ErrorTypeName phase)
  , Document (ErrorBinding phase)
  , Document (ErrorTypeBinding phase)
  , Binds (ErrorBinding phase) (ErrorType phase)
  , Binds (ErrorTypeBinding phase) (ErrorKind phase)
  ) => Document (ErrorFor phase) where
  document = ppError (PPError document document document document document document document document)

-- | Helper to lift errors that are provided as plain 'Doc's to 'Errors'.
liftEMsg :: Either Doc a -> Either (ErrorFor phase) a
liftEMsg e = case e of
  Right a
    -> Right a

  Left doc
    -> Left . EMsg $ doc
