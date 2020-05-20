{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , GADTs
  , ConstraintKinds
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
  ( TypeReductionCtx (..)
  , topTypeReductionCtx

  , reduceType
  , reduceTypeStep
  )
  where

import PL.Bindings
import PL.Binds
import PL.Binds.Ix
import PL.Error
import PL.ExprLike
import PL.Name
import PL.Type
import PL.TyVar
import PL.TypeCtx
import PLPrinter
import PL.FixPhase

import Control.Applicative
import Control.Arrow (second)
import Data.Monoid
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | TypeReductionCtx contains information used/ generated when reducing an
-- expression.
data TypeReductionCtx = TypeReductionCtx
  { _typeReductionTypeBindings :: Bindings Type -- ^ Types that are either Bound or Unbound by an outer type abstraction.
  , _typeReductionTypeCtx      :: TypeCtx       -- ^ Associated named types to their TypeInfo definitions.
  , _typeReductionGas          :: Maybe Int     -- ^ Proportional to the amount of reduction steps allowed before reduction is aborted. All reduction steps should terminate eventually however this parameter can be used to detect bugs, I.E. diverging reduction or inefficient reduction paths.
  }

-- | Reduce a top-level type with a TypeCtx mapping type-names to type
-- definitions.
topTypeReductionCtx
  :: TypeCtx
  -> TypeReductionCtx
topTypeReductionCtx typeCtx = TypeReductionCtx
  { _typeReductionTypeBindings = emptyBindings
  , _typeReductionTypeCtx      = typeCtx
  , _typeReductionGas          = Just 100
  }

-- | True when the ReductionCtx has been provided limited gas which has been
-- depleted.
hitTypeReductionLimit
  :: TypeReductionCtx
  -> Bool
hitTypeReductionLimit ctx = case _typeReductionGas ctx of
  Nothing
    -> False
  Just g
    -> g <= 0

-- | If the amount of reductions has a gas limit, reduce the amount available.
reduceTypeReductionLimit
  :: TypeReductionCtx
  -> TypeReductionCtx
reduceTypeReductionLimit ctx = ctx{_typeReductionGas = fmap (subtract 1) $ _typeReductionGas ctx}

-- | Query for the Bound/Unbound Type associated with a type variable binding.
--
-- Fails when the variable supplied is too large.
lookupTyVarBinding
  :: TypeReductionCtx
  -> TyVar
  -> Either (Error expr Type pattern TypeCtx) (Binding Type)
lookupTyVarBinding ctx b =
  let ix       = bindDepth b
      bindings = _typeReductionTypeBindings ctx
   in case safeIndex (Proxy :: Proxy TyVar) bindings ix of
        -- We've been asked to bind a type further away than there are
        -- abstractions.
        -- This indicates either a logic error in the reduction or a failure
        -- to type-check/ validate the input AST for this case.
        --
        -- TODO:
        -- - Are invalid bindings caught at the type-checking phase?
        -- - Can we check in an earlier phase and avoid doing so here?
        Nothing
          -> Left . EBindTypeLookupFailure ix $ bindings

        Just binding
          -> Right binding

-- | Context after a Type is applied.
underTypeAppliedType
  :: Type
  -> TypeReductionCtx
  -> TypeReductionCtx
underTypeAppliedType appliedType ctx = ctx{_typeReductionTypeBindings = bind appliedType . _typeReductionTypeBindings $ ctx}

-- | Context under some type abstraction where the type variable has _not_ been
-- bound.
underTypeTypeAbstraction
  :: TypeReductionCtx
  -> TypeReductionCtx
underTypeTypeAbstraction ctx = ctx{_typeReductionTypeBindings = unbound . bury . _typeReductionTypeBindings $ ctx}


-- | Reducing a Type means to walk down the AST that is presumed to be:
-- - Type checked
-- - Kind checked
-- - Stripped of unecessary extensions such as comments
--
-- And attempt to reduce it to it's normal form.
--
-- Reducing means to:
--
-- - Replace bound typebindings with the bound type.
-- - Leave unbound bindings intact, ensuring they are adjusted to point at the
--   correct abstraction where necessary.
--
-- - Reducing has strict semantics meaning:
--   - Arguments to type application are reduced themselves before being
--   substituted into the body of the type function.
reduceType
  :: TypeReductionCtx
  -> Type
  -> Either (Error expr Type pattern TypeCtx) Type
reduceType ctx ty
  | hitTypeReductionLimit ctx
   = Left . ETypeReductionLimitReached $ ty

  | otherwise
   = do -- Apply the reduce step until the expression no longer changes.
        -- This requires that reduction eventually converges - diverging will lead to
        -- non-termination.
        -- TODO: reduceTypeStep only reduces one level under types for each call. If
        -- we assume the type reduces, we should reduce faster by recursing with
        -- reduceTypeWith instead.
        reducedTy <- reduceTypeStep ctx ty
        if reducedTy == ty
          then pure ty
          else reduceType (reduceTypeReductionLimit ctx) reducedTy

-- | 'reduceType' a single step with a collection of initial type bindings as if
-- Types have already been applied to an outer type lambda abstraction.
reduceTypeStep
  :: TypeReductionCtx
  -> Type
  -> Either (Error expr Type pattern TypeCtx) Type
reduceTypeStep ctx ty = case ty of

  -- TypeBindings are substituted if they have been bound.
  TypeBinding b
    -> do binding <- lookupTyVarBinding ctx b
          pure $ case binding of
            -- If no type has been bound there is no further reduction
            -- possible.
            -- E.G. We're looking at '?0' in a type lambda that has not been
            -- applied.
            Unbound
              -> TypeBinding b

            -- If a type has been bound, we substitute it for the binding.
            -- We assume strict evaluation of the type and so don't reduce it
            -- again.
            Bound boundTy
              -> boundTy

  -- Reduce using strictish semantics:
  -- - First reduce a step under the applied type
  -- - Then reduce a step under the type function
  -- - Then bind the type and reduce the entire type a step.
  --
  -- Typechecking _should_ have ensured the type function is an arrow kind that
  -- matches the type. It should therefore only be a type lambda or a type binding
  -- to a (possibly unknown) function.
  TypeApp f x
    -> do x' <- reduceTypeStep ctx x
          f' <- reduceTypeStep ctx f
          case f' of
            -- Type lambdas are reduced by binding applied values into the body
            -- and reducing.
            TypeLam _absKind fBodyType
              -> reduceTypeStep (underTypeAppliedType x' ctx) fBodyType

            -- The function we're applying is 'higher order' - it is sourced
            -- from a function itself.
            --
            -- If a type was bound it should have been substituted in the
            -- function reduction and so we can assume it is unbound and do
            -- nothing except reduce the argument a step.
            -- If we're wrong an additional reduceStep on the
            -- application should make progress.
            TypeBinding tyVar
              -> pure $ TypeApp (TypeBinding tyVar) x'

            -- We're applying a named type which should be looked up in the type
            -- context.
            --
            -- Similar to TypeBindings - this should have been reduced away
            -- already.
            Named n
              -> case lookupTypeNameInfo n (_typeReductionTypeCtx ctx) of
                   Nothing
                     -> Left $ EMsg $ text "Cant reduce type application to a name which does not exist in the context"

                   Just _ti
                     -> Right $ TypeApp f' x'

            -- An error here indicates type/kind checking has not been performed/ has
            -- been performed incorrectly as the type in type function
            -- position is not a lambda.
            _ -> Left $ ETypeAppLambda f

  -- Reduce a single step under the type lambda type.
  TypeLam takeKind tyBody
    -> (TypeLam takeKind) <$> reduceTypeStep (underTypeTypeAbstraction ctx) tyBody

  -- Reduce a single step under the big arrow types.
  BigArrow fromKind toTy
    -- TODO: Should bindings be wrapped with 'unbound'?
    -> BigArrow fromKind <$> reduceTypeStep ctx toTy

  -- Names are reduced by substituting their definition.
  -- TODO:
  -- - Should we reduce the definition of non-recursive types?
  -- - Should we reduce the definition of recursive types (being careful not to
  --   expand the recursion?)
  Named n
    -> case lookupTypeNameInitialInfo n (_typeReductionTypeCtx ctx) of
         Nothing
           -> Left $ ETypeNotDefined n (_typeReductionTypeCtx ctx)

         Just (TypeInfo NonRec k ty)
           -> Right ty

         -- Do not expose the definition of recursive types so that recursive
         -- reduction terminates!
         Just (TypeInfo Rec k ty)
           -> pure $ Named n

  -- Reduce a single step under the arrow type.
  Arrow from to
    -> Arrow <$> reduceTypeStep ctx from
             <*> reduceTypeStep ctx to

  -- Reduce a single step under the sum types.
  SumT types
    -> SumT <$> mapM (reduceTypeStep ctx) types

  -- Reduce a single step under the product types.
  ProductT types
    -> ProductT <$> mapM (reduceTypeStep ctx) types

  -- Reduce a single step under the union types.
  UnionT types
    -> (UnionT . Set.fromList) <$> mapM (reduceTypeStep ctx) (Set.toList types)

  _ -> error "Non-exhaustive pattern in type reduction"

