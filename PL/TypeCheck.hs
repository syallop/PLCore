{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : PL.TypeCheck
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Data structures and functions for type-checking.

TypeCheckCtx maintains state for:
- Expression bindings (such as variables) type.
- Type Bindings (such as type variables) kind.
- Types that have been applied.
- Type definitions in scope
- The Type of expressions that are refered to by ContentNames

By co-ordinating BindCtx, Bindings, TypeCtx.

Functions are provided to:
- Check the type of top-level expressions
- Validate whether types are equal
- Check types Kinds
- Query variables type/ kind
- Query named expressions types
- Reduce Type definitions
- And modify the type context to check types under abstractions.

The internals of TypeCheckCtx are exposed so they can be provided to external
functions which have not yet been folded into this api.

-}

module PL.TypeCheck
  ( TypeCheckCtx (..)

  , topTypeCheckCtx

  , underExpressionAbstraction
  , underExpressionAbstractions
  , underTypeAbstraction

  , underAppliedType

  , kindCheck

  , checkEqual

  , lookupVarType
  , lookupExprContentType
  , lookupTypeContentKind
  , lookupTypeContentType

  , reduceTypeUnderCtx
  )
  where

import PL.Bindings
import PL.Binds
import PL.Error
import PL.Kind
import PL.ReduceType
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Name
import PL.Var
import PL.FixPhase

import PLPrinter

import Data.Map (Map)
import qualified Data.Map as Map

-- | TypeCheckCtx contains information used/ generated while type-checking an
-- expression.
data TypeCheckCtx = TypeCheckCtx
  { _exprBindCtx    :: BindCtx Var Type     -- ^ Associate expression bindings to their types. E.G. Var 0 to Bool.
  , _typeBindCtx    :: BindCtx TyVar Kind   -- ^ Associate type bindings to their kinds. E.G. TyVar 0 to (Kind->Kind).
  , _typeBindings   :: Bindings Type        -- ^ When type-checking under a binder, types are either bound with some known type or are unbound

  , _selfType       :: Maybe Type           -- ^ The asserted Type of an outer self-type.
  , _selfKind       :: Maybe Kind           -- ^ The asserted Kind of an outer self-type.

  , _typeCtx        :: TypeCtx              -- ^ Associated named types to their TypeInfo definitions
  , _contentHasType :: Map ContentName Type -- ^ Cache a mapping of known expression content names to their checked Type. We have a cache here to avoid doing IO in type-checking which feels wrong. It implies we must resolve names in an earlier phase.
  , _contentHasKind :: Map ContentName Kind -- ^ Cache a mapping of known type content names to their checked Kind. We have a cache here to avoid doing IO in type-checking which feels wrong. It implies we must resolve names in an earlier phase.
  , _contentIsType  :: Map ContentName Type -- ^ Cache a mapping of known type content names to their type definition. We have a cache here to avoid doing IO in type-checking which feels wrong. It implies we must resolve names in an earlier phase.
  }
  deriving Show

-- | Type check a top-level expression with a TypeCtx mapping type names to type definitions.
topTypeCheckCtx
  :: TypeCtx
  -> TypeCheckCtx
topTypeCheckCtx typeCtx = TypeCheckCtx
  { _exprBindCtx    = emptyCtx
  , _typeBindCtx    = emptyCtx
  , _typeBindings   = emptyBindings
  , _selfKind       = Nothing
  , _selfType       = Nothing
  , _typeCtx        = typeCtx
  , _contentHasType = mempty
  , _contentHasKind = mempty
  , _contentIsType  = mempty
  }

-- | Context under an abstraction of expressions with a given type.
underExpressionAbstraction
  :: Type
  -> TypeCheckCtx
  -> TypeCheckCtx
underExpressionAbstraction underAbstractionType ctx = ctx{_exprBindCtx = addBinding underAbstractionType $ _exprBindCtx ctx}

-- | Context under multiple abstractions of expressions with given types.
underExpressionAbstractions
  :: [Type]
  -> TypeCheckCtx
  -> TypeCheckCtx
underExpressionAbstractions underAbstractionTypes ctx = ctx{_exprBindCtx = addBindings underAbstractionTypes $ _exprBindCtx ctx}

-- | Context under an abstraction of types with a given kind.
underTypeAbstraction
  :: Kind
  -> TypeCheckCtx
  -> TypeCheckCtx
underTypeAbstraction underAbstractionKind ctx = ctx{ _typeBindCtx  = addBinding underAbstractionKind $ _typeBindCtx ctx
                                                   , _typeBindings = unbound . bury . _typeBindings $ ctx
                                                   }

-- | Check that a type has a valid kind (and return it) under a context of type
-- bindings and definitions.
kindCheck
  :: Type
  -> TypeCheckCtx
  -> Either Error Kind
kindCheck t ctx = typeKind (_typeBindCtx ctx) (_contentHasKind ctx) (_selfKind ctx) (_typeCtx ctx) t

-- | Context after a Type with Kind is applied.
--
-- The Kind is assumed to be correct and should probably be equal to the result
-- of kindCheck.
underAppliedType
  :: (Type, Kind)
  -> TypeCheckCtx
  -> TypeCheckCtx
underAppliedType (t,hasKind) ctx = ctx
  {_typeBindCtx  = addBinding hasKind $ _typeBindCtx ctx
  ,_typeBindings = bind t $ _typeBindings ctx
  }

-- | Check that two types are equal under a context.
checkEqual
  :: Type
  -> Type
  -> TypeCheckCtx
  -> Either Error Bool
checkEqual aTy bTy ctx = typeEq (_typeBindCtx ctx) (_typeBindings ctx) (_selfType ctx) (_typeCtx ctx) (_contentIsType ctx) aTy bTy

-- | Lookup the Type associated with an expression variable.
lookupVarType
  :: ( ErrorType phase ~ Type
     , ErrorTypeCtx phase ~ TypeCtx
     , ErrorBinding phase ~ Var
     )
  => Var
  -> TypeCheckCtx
  -> Either (ErrorFor phase) Type
lookupVarType v ctx = case lookupBindingTy v (_exprBindCtx ctx) of
  Nothing
    -> Left $ EBindCtxExprLookupFailure (fromEnum v) (_exprBindCtx ctx)
  Just t
    -> Right t

-- | Lookup the Type associated with a named expression.
lookupExprContentType
  :: ( ErrorType phase ~ Type
     , ErrorTypeCtx phase ~ TypeCtx
     )
  => ContentName
  -> TypeCheckCtx
  -> Either (ErrorFor phase) Type
lookupExprContentType name ctx = case Map.lookup name . _contentHasType $ ctx of
  Nothing
    -> Left . EMsg . mconcat $
         [ text "Could not find a type association for content named: "
         , lineBreak
         , indent1 . string . show $ name
         , lineBreak
         ]

  Just t
    -> Right t

-- | Lookup the Kind associated with a named type.
lookupTypeContentKind
  :: ( ErrorType phase ~ Type
     , ErrorTypeCtx phase ~ TypeCtx
     )
  => ContentName
  -> TypeCheckCtx
  -> Either (ErrorFor phase) Kind
lookupTypeContentKind name ctx = case Map.lookup name . _contentHasKind $ ctx of
  Nothing
    -> Left . EMsg . mconcat $
         [ text "Could not find a kind association for content named: "
         , lineBreak
         , indent1 . string . show $ name
         , lineBreak
         ]

  Just k
    -> Right k

-- | Lookup the Type associated with a named type.
lookupTypeContentType
  :: ( ErrorType phase ~ Type
     , ErrorTypeCtx phase ~ TypeCtx
     )
  => ContentName
  -> TypeCheckCtx
  -> Either (ErrorFor phase) Type
lookupTypeContentType name ctx = case Map.lookup name . _contentIsType $ ctx of
  Nothing
    -> Left . EMsg . mconcat $
         [ text "Could not find the type definition for content named:"
         , lineBreak
         , indent1 . string . show $ name
         , lineBreak
         ]

  Just t
    -> Right t

-- | Recursivly reduce a type to it's initial definition.
-- Meaning all non-recursive types are substituted for their definition.
-- Recursive types are not substituted.
--
-- If used (very) carefully this function can be used to check equality of
-- recursive types. Probably not in the general case.
reduceTypeUnderCtx
  :: Type
  -> TypeCheckCtx
  -> Either Error Type
reduceTypeUnderCtx t ctx = reduceType (TypeReductionCtx (_typeBindings ctx) (_selfType ctx) (_typeCtx ctx) (Just 128)) t

