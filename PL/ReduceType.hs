{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
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
import PL.Name
import PL.Type
import PL.TypeCtx
import PL.FixType
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
  :: forall tb
   . ( Ord tb
     , BindingIx tb
     )
  => TypeCtx tb
  -> Type tb
  -> Either (Error tb) (Type tb)
reduceType = reduceTypeRec emptyBindings
  where

  -- Recursively reduce a type until it no longer reduces.
  reduceTypeRec :: Bindings (Type tb) -> TypeCtx tb -> Type tb -> Either (Error tb) (Type tb)
  reduceTypeRec bindings typeNameCtx ty = do
    reducedTy <- reduceTypeStep bindings typeNameCtx ty
    if reducedTy == ty then pure ty else reduceTypeRec bindings typeNameCtx reducedTy

-- Reduce a type by a single reduction step.
reduceTypeStep
  :: forall tb
   . ( Ord tb
     , BindingIx tb
     )
  => Bindings (Type tb)
  -> TypeCtx tb
  -> Type tb
  -> Either (Error tb) (Type tb)
reduceTypeStep bindings typeNameCtx ty = case unfixType ty of

  -- Bindings reduce to whatever they've been bound to, if they've been bound that is.
  TypeBinding b
    -> pure $ case index (Proxy :: Proxy tb) bindings (bindDepth b) of
         Unbound   -> fixType $ TypeBinding b
         Bound ty' -> ty' -- maybe should reduce again?

  TypeApp f x
    -> do x' <- reduceTypeStep bindings typeNameCtx x
          f' <- reduceTypeStep bindings typeNameCtx f
          case unfixType f' of
            TypeLam _ fTy
              -> reduceTypeStep (bind x' bindings) typeNameCtx fTy

            Named n
              -> case lookupTypeNameInfo n typeNameCtx of
                   Nothing -> Left $ EMsg $ text "Cant reduce type application to a name which does not exist in the context"

                   -- f is a named type we know of
                   -- we've assumed everything has been kind-checked so we're
                   -- not gonna reduce further for reasons. Mainly we would
                   -- like to terminate...
                   Just ti -> Right $ fixType $ TypeApp f' x'

            _ -> Left $ ETypeAppLambda f

  -- Reduce under a lambda by noting the abstraction is buried and Unbound
  -- TODO: Maybe dont reduce under
  TypeLam takeKind ty
    -> (fixType . TypeLam takeKind) <$> reduceTypeStep (unbound $ bury bindings) typeNameCtx ty

  -- TODO: Remain unconvinced by this definition.
  -- Reduce the rhs by noting the abstraction is Unbound
  BigArrow from to
    -> (fixType . BigArrow from) <$> reduceTypeStep (unbound bindings) typeNameCtx to

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
    -> (\a b -> fixType $ Arrow a b) <$> reduceTypeStep bindings typeNameCtx from <*> reduceTypeStep bindings typeNameCtx to

  SumT types
    -> (fixType . SumT) <$> mapM (reduceTypeStep bindings typeNameCtx) types

  ProductT types
    -> (fixType . ProductT) <$> mapM (reduceTypeStep bindings typeNameCtx) types

  UnionT types
    -> (fixType . UnionT . Set.fromList) <$> mapM (reduceTypeStep bindings typeNameCtx) (Set.toList types)

