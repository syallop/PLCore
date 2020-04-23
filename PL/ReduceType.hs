{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , GADTs
  #-}
{-|
Module      : PL.ReduceType
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Duplication of Reduce but acting at the type level. Currently has the right
to behave differently and terminate on types that could be otherwise reduced.
-}
module PL.ReduceType
  (reduceType
  ,reduceTypeStep
  )
  where

import PL.Bindings
import PL.Binds
import PL.Binds.Ix
import PL.Error
import PL.ExprLike
import PL.Name
import PL.Type
import PL.TypeCtx
import PLPrinter

import Control.Applicative
import Control.Arrow (second)
import Data.Monoid
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | Reduce a top-level type - A type under no outer abstractions
-- Assume kind checked?
reduceType
  :: forall phase
   . (Eq (NamedExtension phase)
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
     , phase ~ DefaultPhase
     , HasBinding (TypeFor phase) (TypeBindingFor phase)
     )
  => TypeCtx phase
  -> TypeFor phase
  -> Either (Error phase) (TypeFor phase)
reduceType = reduceTypeRec emptyBindings
  where

  -- Recursively reduce a type until it no longer reduces.
  reduceTypeRec :: Bindings (TypeFor phase) -> TypeCtx phase -> TypeFor phase -> Either (Error phase) (TypeFor phase)
  reduceTypeRec bindings typeNameCtx ty = do
    reducedTy <- reduceTypeStep bindings typeNameCtx ty
    if reducedTy == ty then pure ty else reduceTypeRec bindings typeNameCtx reducedTy

-- Reduce a type by a single reduction step.
reduceTypeStep
  :: forall phase
   . phase ~ DefaultPhase
  => Bindings (TypeFor phase)
  -> TypeCtx phase
  -> TypeFor phase
  -> Either (Error phase) (TypeFor phase)
reduceTypeStep bindings typeNameCtx ty = case ty of

  -- Bindings reduce to whatever they've been bound to, if they've been bound that is.
  TypeBinding b
    -> pure $ case index (Proxy :: Proxy (TypeBindingFor phase)) bindings (bindDepth b) of
         Unbound   -> TypeBinding b
         Bound ty' -> ty' -- maybe should reduce again?

  TypeApp f x
    -> do x' <- reduceTypeStep bindings typeNameCtx x
          f' <- reduceTypeStep bindings typeNameCtx f
          case f' of
            TypeLam _ fTy
              -> reduceTypeStep (bind x' bindings) typeNameCtx fTy

            Named n
              -> case lookupTypeNameInfo n typeNameCtx of
                   Nothing -> Left $ EMsg $ text "Cant reduce type application to a name which does not exist in the context"

                   -- f is a named type we know of
                   -- we've assumed everything has been kind-checked so we're
                   -- not gonna reduce further for reasons. Mainly we would
                   -- like to terminate...
                   Just ti -> Right $ TypeApp f' x'

            _ -> Left $ ETypeAppLambda f

  -- Reduce under a lambda by noting the abstraction is buried and Unbound
  -- TODO: Maybe dont reduce under
  TypeLam takeKind ty
    -> (TypeLam takeKind) <$> reduceTypeStep (unbound $ bury bindings) typeNameCtx ty

  -- TODO: Remain unconvinced by this definition.
  -- Reduce the rhs by noting the abstraction is Unbound
  BigArrow from to
    -> (BigArrow from) <$> reduceTypeStep (unbound bindings) typeNameCtx to

  -- Dont reduce names
  Named n
    -> case lookupTypeNameInitialInfo n typeNameCtx of
         Nothing
           -> Left $ EMsg $ text "Name does not exist in type reduction"

         Just (TypeInfo NonRec k ty)
           -> Right ty

         -- Dont reduce recursive types
         {-Just (TypeInfo Rec k ty)-}
           {--> Right $ Named n-}
         Just (TypeInfo Rec k ty)
           -> Right ty

  Arrow from to
    -> Arrow <$> reduceTypeStep bindings typeNameCtx from <*> reduceTypeStep bindings typeNameCtx to

  SumT types
    -> SumT <$> mapM (reduceTypeStep bindings typeNameCtx) types

  ProductT types
    -> ProductT <$> mapM (reduceTypeStep bindings typeNameCtx) types

  UnionT types
    -> (UnionT . Set.fromList) <$> mapM (reduceTypeStep bindings typeNameCtx) (Set.toList types)

  _ -> error "Non-exhaustive pattern in type reduction"
