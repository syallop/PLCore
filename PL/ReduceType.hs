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
  ( reduceType
  , reduceTypeWith
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
import PL.TypeCtx
import PLPrinter

import Control.Applicative
import Control.Arrow (second)
import Data.Monoid
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as Text

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
  :: TypeCtx DefaultPhase
  -> TypeFor DefaultPhase
  -> Either (Error expr Type pattern) (TypeFor DefaultPhase)
reduceType typeCtx typ = reduceTypeWith emptyBindings typeCtx (Just 100) typ

-- | 'reduceType' with a collection of initial bindings as if Types have already
-- been applied to an outer type lambda abstraction.
reduceTypeWith
  :: Bindings (TypeFor DefaultPhase) -- ^ Bind known and unknown types by their position away
  -> TypeCtx DefaultPhase            -- ^ Known named types
  -> Maybe Int
  -> TypeFor DefaultPhase
  -> Either (Error expr Type pattern) (TypeFor DefaultPhase)
reduceTypeWith bindings typeNameCtx reductionLimit ty
  | reductionLimit == Just 0
   = Left . ETypeReductionLimitReached $ ty

  | otherwise
   = do -- Apply the reduce step until the expression no longer changes.
        -- This requires that reduction eventually converges - diverging will lead to
        -- non-termination.
        -- TODO: We could add a reduction limit here to guard against accidental
        -- diversion.
        -- TODO: reduceTypeStep only reduces one level under types for each call. If
        -- we assume the type reduces, we should reduce faster by recursing with
        -- reduceTypeWith instead.
        reducedTy <- reduceTypeStep bindings typeNameCtx ty
        if reducedTy == ty
          then pure ty
          else reduceTypeWith bindings typeNameCtx (fmap (subtract 1) reductionLimit) reducedTy

-- | 'reduceType' a single step with a collection of initial type bindings as if
-- Types have already been applied to an outer type lambda abstraction.
reduceTypeStep
  :: Bindings (TypeFor DefaultPhase)
  -> TypeCtx DefaultPhase
  -> TypeFor DefaultPhase
  -> Either (Error expr Type pattern) (TypeFor DefaultPhase)
reduceTypeStep bindings typeNameCtx ty = case ty of

  -- TypeBindings are substituted if they have been bound.
  TypeBinding b
    -> let ix = bindDepth b
        in case safeIndex (Proxy :: Proxy (TypeBindingFor DefaultPhase)) bindings ix of
             -- If no type has been bound there is no further reduction
             -- possible.
             -- E.G. We're looking at '?0' in a type lambda that has not been
             -- applied.
             Just Unbound
               -> pure $ TypeBinding b

             -- If a type has been bound, we substitute it for the binding.
             -- We assume strict evaluation of the type and so don't reduce it
             -- again.
             Just (Bound ty')
               -> pure ty'

            -- We've been asked to bind a type further away than there are
            -- type lambda abstractions.
            -- This indicates either a logic error in the reduction or a failure
            -- to type-check/ validate the input AST for this case.
            --
            -- TODO:
            -- - Are invalid bindings caught at the type-checking phase?
            -- - Can we check in an earlier phase and avoid doing so here?
             Nothing
               -> Left . EMsg . mconcat $
                    [ text "Cannot bind a type from "
                    , text . Text.pack . show $ ix
                    , text " type abstractions away as there are not that many type abstractions"
                    ]

  -- Reduce using strictish semantics:
  -- - First reduce a step under the applied type
  -- - Then reduce a step under the type function
  -- - Then bind the type and reduce the entire type a step.
  --
  -- Typechecking _should_ have ensured the type function is an arrow kind that
  -- matches the type. It should therefore only be a type lambda or a type binding
  -- to a (possibly unknown) function.
  TypeApp f x
    -> do x' <- reduceTypeStep bindings typeNameCtx x
          f' <- reduceTypeStep bindings typeNameCtx f
          case f' of
            -- Type lambdas are reduced by binding applied values into the body
            -- and reducing.
            TypeLam _absKind fBodyExpr
              -> reduceTypeStep (bind x' bindings) typeNameCtx fBodyExpr

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
              -> case lookupTypeNameInfo n typeNameCtx of
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
    -> (TypeLam takeKind) <$> reduceTypeStep (unbound $ bury bindings) typeNameCtx tyBody

  -- Reduce a single step under the big arrow types.
  BigArrow fromKind toTy
    -- TODO: Should bindings be wrapped with 'unbound'?
    -> BigArrow fromKind <$> reduceTypeStep bindings typeNameCtx toTy

  -- Names are reduced by substituting their definition.
  -- TODO:
  -- - Should we reduce the definition of non-recursive types?
  -- - Should we reduce the definition of recursive types (being careful not to
  --   expand the recursion?)
  Named n
    -> case lookupTypeNameInitialInfo n typeNameCtx of
         Nothing
           -> Left $ EMsg $ text "Name does not exist in type reduction"

         Just (TypeInfo NonRec k ty)
           -> Right ty

         -- Do not expose the definition of recursive types so that recursive
         -- reduction terminates!
         Just (TypeInfo Rec k ty)
           -> pure $ Named n

  -- Reduce a single step under the arrow type.
  Arrow from to
    -> Arrow <$> reduceTypeStep bindings typeNameCtx from <*> reduceTypeStep bindings typeNameCtx to

  -- Reduce a single step under the sum types.
  SumT types
    -> SumT <$> mapM (reduceTypeStep bindings typeNameCtx) types

  -- Reduce a single step under the product types.
  ProductT types
    -> ProductT <$> mapM (reduceTypeStep bindings typeNameCtx) types

  -- Reduce a single step under the union types.
  UnionT types
    -> (UnionT . Set.fromList) <$> mapM (reduceTypeStep bindings typeNameCtx) (Set.toList types)

  _ -> error "Non-exhaustive pattern in type reduction"

