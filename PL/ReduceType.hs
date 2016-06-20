{-# LANGUAGE ScopedTypeVariables #-}
module PL.ReduceType where

import PL.Binds
import PL.Bindings
import PL.Error
import PL.Type
import PL.TypeCtx
import PL.Name

import Control.Applicative
import Control.Arrow (second)
import Data.Proxy
import qualified Data.Set as Set

-- | Reduce a top-level type - A type under no outer abstractions
-- Assume kind checked?
reduceType :: forall tb. (Ord tb,Show tb,BindingIx tb) => TypeCtx tb -> Type tb -> Either (Error tb) (Type tb)
reduceType = reduceTypeRec emptyBindings
  where

  -- Recursively reduce a type until it no longer reduces.
  reduceTypeRec :: Bindings (Type tb) -> TypeCtx tb -> Type tb -> Either (Error tb) (Type tb)
  reduceTypeRec bindings typeNameCtx ty = do
    reducedTy <- reduceTypeStep bindings typeNameCtx ty
    if reducedTy == ty then pure ty else reduceTypeRec bindings typeNameCtx reducedTy

-- Reduce a type by a single reduction step.
reduceTypeStep :: forall tb. (Ord tb,Show tb,BindingIx tb) => Bindings (Type tb) -> TypeCtx tb -> Type tb -> Either (Error tb) (Type tb)
reduceTypeStep bindings typeNameCtx ty = case ty of

  -- Bindings reduce to whatever they've been bound to, if they've been bound that is.
  TypeBinding b
    -> pure $ case index (Proxy :: Proxy tb) bindings (bindDepth b) of
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
                   Nothing -> error "Cant reduce type application to a name which does not exist in the context"

                   -- f is a named type we know of
                   -- we've assumed everything has been kind-checked so we're
                   -- not gonna reduce further for reasons. Mainly we would
                   -- like to terminate...
                   Just ti -> Right $ TypeApp f' x' 

            _ -> error $ "Cant reduce type application of non-lambda term: f: " ++ show f'

  -- Reduce under a lambda by noting the abstraction is buried and Unbound
  -- TODO: Maybe dont reduce under
  TypeLam takeKind ty
    -> TypeLam takeKind <$> reduceTypeStep (unbound $ bury bindings) typeNameCtx ty

  -- TODO: Remain unconvinced by this definition.
  -- Reduce the rhs by noting the abstraction is Unbound
  BigArrow from to
    -> BigArrow from <$> reduceTypeStep (unbound bindings) typeNameCtx to

  -- Dont reduce names
  Named n
    -> case lookupTypeNameInitialInfo n typeNameCtx of
         Nothing
           -> error "Name does not exist in type reduction"

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




