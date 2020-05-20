{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , LambdaCase
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  #-}
{-|
Module      : PL.Reduce
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Reduce expressions by maintaining a binding ctx and performing substitution
and recursive reduction when necessary.
-}
module PL.Reduce
  ( ReductionCtx (..)
  , topReductionCtx

  , reduce
  , reduceStep

  , reducePossibleCaseBranchesStep
  , reducePossibleCaseRHSStep

  -- TODO: Relocate near case?
  , tryBranches
  , tryBranch
  , patternBinding
  , patternBindings
  , exprEq
  )
  where

import PL.Bindings
import PL.Binds
import PL.Binds.Ix
import PL.Case
import PL.Error
import PL.Expr
import PL.Name
import PL.Type
import PL.Var
import PL.TyVar
import PL.TypeCtx
import PL.ReduceType
import PL.Pattern

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Proxy

import qualified Data.Text as Text
import qualified Data.Set as Set

import PLPrinter

-- | ReductionCtx contains information used/ generated when reducing an
-- expression.
data ReductionCtx = ReductionCtx
  { _reductionExprBindings :: Bindings Expr -- ^ Expressions that are either Bound or Unbound by an outer abstraction.
  , _reductionTypeBindings :: Bindings Type -- ^ Types that are either Bound or Unbound by an outer type abstraction.
  , _reductionTypeCtx      :: TypeCtx       -- ^ Associated named types to their TypeInfo definitions.
  , _reductionGas          :: Maybe Int     -- ^ Proportional to the amount of reduction steps allowed before reduction is aborted. All reduction steps should terminate eventually however this parameter can be used to detect bugs, I.E. diverging reduction or inefficient reduction paths.
  }
  deriving Show

-- | Reduce a top-level expression with a TypeCtx mapping type-names to type
-- definitions.
topReductionCtx
  :: TypeCtx
  -> ReductionCtx
topReductionCtx typeCtx = ReductionCtx
  { _reductionExprBindings = emptyBindings
  , _reductionTypeBindings = emptyBindings
  , _reductionTypeCtx      = typeCtx
  , _reductionGas          = Just 100
  }

-- | True when the ReductionCtx has been provided limited gas which has been
-- depleted.
hitReductionLimit
  :: ReductionCtx
  -> Bool
hitReductionLimit ctx = _reductionGas ctx <= Just 0

-- | If the amount of reductions has a gas limit, reduce the amount available.
reduceReductionLimit
  :: ReductionCtx
  -> ReductionCtx
reduceReductionLimit ctx = ctx{_reductionGas = fmap (subtract 1) $ _reductionGas ctx}

-- | Context under some abstraction where the variable has _not_ been bound.
underExpressionAbstraction
  :: ReductionCtx
  -> ReductionCtx
underExpressionAbstraction ctx = ctx{_reductionExprBindings = unbound . bury . _reductionExprBindings $ ctx}

-- | Context after an Expr is applied.
underAppliedExpression
  :: Expr
  -> ReductionCtx
  -> ReductionCtx
underAppliedExpression appliedExpr ctx = ctx{_reductionExprBindings = bind appliedExpr . _reductionExprBindings $ ctx}

-- | Context after multiple Exprs are applied left-to-right.
-- (I.E. right-most becomes the nearest binding).
underAppliedExpressions
  :: [Expr]
  -> ReductionCtx
  -> ReductionCtx
underAppliedExpressions appliedExprs ctx = ctx{_reductionExprBindings = bindAll appliedExprs . _reductionExprBindings $ ctx}

-- | Context under some type abstraction where the type variable has _not_ been
-- bound.
underTypeAbstraction
  :: ReductionCtx
  -> ReductionCtx
underTypeAbstraction ctx = ctx{_reductionTypeBindings = unbound . bury . _reductionTypeBindings $ ctx}

-- | Context after a Type is applied.
underAppliedType
  :: Type
  -> ReductionCtx
  -> ReductionCtx
underAppliedType appliedType ctx = ctx{_reductionTypeBindings = bind appliedType . _reductionTypeBindings $ ctx}

-- | Query for the Bound/Unbound Expr associated with a variable binding.
--
-- Fails when the variable supplied is too large.
lookupVarBinding
  :: ReductionCtx
  -> Var
  -> Either (Error Expr Type Pattern TypeCtx) (Binding Expr)
lookupVarBinding ctx b =
  let ix       = bindDepth b
      bindings = _reductionExprBindings ctx
   in case safeIndex (Proxy :: Proxy Var) bindings ix of
        -- We've been asked to bind an expression further away than there are
        -- lambda abstractions.
        -- This indicates either a logic error in the reduction or a failure
        -- to type-check/ validate the input AST for this case.
        --
        -- E.G. '\Foo 99'.
        --
        -- TODO:
        -- - Are invalid bindings caught at the type-checking phase?
        -- - Can we check in an earlier phase and avoid doing so here?
        Nothing
          -> Left . EBindExprLookupFailure ix $ bindings

        Just binding
          -> Right binding

-- | Create a context for reducing types, built upon the current context for
-- building expressions.
--
-- This is what allows types at the expression level to be passed into types.
toTypeReductionCtx
  :: ReductionCtx
  -> TypeReductionCtx
toTypeReductionCtx ctx = TypeReductionCtx (_reductionTypeBindings ctx) (_reductionTypeCtx ctx) (_reductionGas ctx)

-- | Reducing an 'Expr'ession means to walk down the AST that is presumed to be:
-- - Type checked
-- - Stripped of unnecessary extensions such as comments
--
-- And attempt to reduce it to it's normal form.
--
-- Some types of errors are currently returned that should have been caught at
-- an earlier phase (such as typechecking). Some are:
-- - Refering to abstractions further away than exist
-- - Applying non-function expressions to expressions
-- Ideally the phase would be adjusted to more strongly require these conditions
-- have already been handled.
--
-- Reducing means to:
--
-- - Replace bound bindings with the bound expression.
--   E.G. @ '(\Nat. 0) (+0 (*))' should reduce to '+0 (*)'
--
-- - Leave unbound bindings intact, ensuring they are adjusted to point at the
--   correct abstraction where necessary.
--
-- - Reducing has strict semantics meaning:
--   - Arguments to function application are reduced themselves before being
--   substituted into the body of the function.
--   - Case analysis reduces it's scrutinee fully before proceeding with
--   matches.
reduce
  :: ReductionCtx
  -> Expr
  -> Either (Error Expr Type Pattern TypeCtx) Expr
reduce ctx initialExpr
  | hitReductionLimit ctx
   = Left . EMsg . mconcat $
       [ text "Reduction limit reached when reducing expression. Got: "
       , text . Text.pack . show $ initialExpr
       ]
  | otherwise
   = do -- Apply the reduce step until the expression no longer changes.
        -- This requires that reduction eventually converges - diverging will lead to
        -- non-termination.
        -- TODO: reduceStep only reduces one level under expressions for each call. If
        -- we assume the expression reduces, we should reduce faster by recursing with
        -- reduce instead.
        reducedExpr <- reduceStep ctx initialExpr
        if reducedExpr == initialExpr
          then pure initialExpr
          else reduce (reduceReductionLimit ctx) reducedExpr

-- | 'reduce' a single step with a collection of initial bindings as if Expressions and Types
-- have already been applied to an outer lambda abstraction.
reduceStep
  :: ReductionCtx
  -> Expr
  -> Either (Error Expr Type Pattern TypeCtx) Expr
reduceStep ctx initialExpr = case initialExpr of
  -- TODO: Consider whether/ where we should reduce types.

  -- Bindings are substituted if they have been bound.
  Binding b
    -> do binding <- lookupVarBinding ctx b
          pure $ case binding of
            -- If no expression has been bound there is no further reduction
            -- possible.
            -- E.G. We're looking at '0' in a lambda that has not been applied: '\Foo 0`
            Unbound
              -> Binding b

          -- If an expression has been bound, we substitute it for the binding.
          -- We assume strict evaluation of the expression and so don't reduce
          -- it again.
            Bound boundExpr
              -> boundExpr

  -- ContentBindings are not (currently) reduced - they're sticking points for
  -- evaluation.
  --
  -- They _could_ be as the lack of self-reference and mutual recursion means
  -- reduction would terminate.
  --
  -- We choose not to as:
  -- - We expect them to be substituted in the evaluation phase.
  -- - Leaving un-reduced allows them to be made recursive/ self-referential in
  --  the future without changing behaviour here.
  -- - The final result of a reduction may be stored and currently serves the
  --   purpose of both the 'compiled' and human(ish) readable form. Some
  --   expressions would look unrecognisable after reducing under names making
  --   modification hard. It is likely we'll end up storing multiple
  --   representations of the 'same' code to deal with this. For now, we
  --   prioritise readability over full reduction.
  ContentBinding c
    -> pure $ ContentBinding c

  -- Reduce a single step under the sum expression.
  Sum sumExpr sumIx sumTy
    -> Sum <$> reduceStep ctx sumExpr
           <*> pure sumIx
           <*> mapM (reduceType (toTypeReductionCtx ctx)) sumTy

  -- Reduce a single step under all product expressions.
  Product productExprs
    -> Product <$> mapM (reduceStep ctx) productExprs

  -- Reduce a single step under the union expression.
  Union unionExpr unionTyIx unionTys
    -> Union <$> reduceStep ctx unionExpr
             <*> reduceType (toTypeReductionCtx ctx) unionTyIx
             <*> ((fmap Set.fromList) <$> mapM (reduceType (toTypeReductionCtx ctx)) . Set.toList $ unionTys)

  -- Reduce a single step under the lambda expression.
  Lam takeTy lamExpr
    -> Lam <$> reduceType (toTypeReductionCtx ctx) takeTy
           <*> reduceStep (underExpressionAbstraction ctx) lamExpr

  -- Reduce using strictish semantics:
  -- - First reduce a step under the applied expression
  -- - Then reduce a step under the function
  -- - Then bind the expression and reduce the entire expression a step.
  --
  -- Typechecking _should_ have ensured the function is an arrow type that
  -- matches the expression. It should therefore only be a lambda or a binding
  -- to a (possibly unknown) function.
  App f x
    -> do x' <- reduceStep ctx x
          f' <- reduceStep ctx f
          case f' of
            -- Lambdas are reduced by binding applied values into the body and
            -- reducing.
            Lam _abs fBodyExpr
              -> reduceStep (underAppliedExpression x' ctx) fBodyExpr

            -- The function we're applying is 'higher order' - it is sourced
            -- from a function itself.
            --
            -- If an expression was bound it should have been substituted in the
            -- function reduction and so we can assume it is unbound and do
            -- nothing except reduce the argument a step.
            -- If we're wrong an additional reduceStep on the
            -- application should make progress.
            Binding var
              -> pure $ App (Binding var) x'

            -- An error here indicates type checking has not been performed/ has
            -- been performed incorrectly as the expression in function
            -- position is not a lambda.
            _ -> Left $ EMsg $ text "Can't reduce because the expression in function position of an application isn't a lambda"

  -- Reduce a single step under the big lambda expression.
  BigLam takeKind lamExpr
    -> BigLam <$> pure takeKind
              <*> reduceStep (underTypeAbstraction ctx) lamExpr

  -- Reduce using strictish semantics:
  -- - First reduce a step under the applied type
  -- - Then reduce a step under the function
  -- - Then bind the type in the expression body and reduce it a step.
  --
  -- Type/ kind checking _should_ have ensured the function is a BigArrow type
  -- that matches the type. It should therefore only be a BigLambda or a
  -- typebinding to a (possibly) unknown function.
  BigApp f ty
    -> do -- TODO:
          -- - Should we reduce the type before applying it?
          -- - Should we be reducing types everywhere else inside expression
          --   reduction?
          ty' <- reduceType (toTypeReductionCtx ctx) ty
          f'  <- reduceStep ctx f
          case f' of
            -- Big Lambdas are reduced by binding applied types into the body
            -- and reducing.
            BigLam _absKind fBodyExpr
              -> reduceStep (underAppliedType ty' ctx) fBodyExpr

            -- The function we're applying is 'higher order' - it is sourced
            -- from a function itself.
            --
            -- If a type was bound it should have been substituted in the
            -- function reduction and so we can assume it is unbound and do
            -- nothing except reduce the argument a step.
            -- If we're wrong an additional reduceStep on the
            -- application should make progress.
            Binding var
              -> pure $ BigApp (Binding var) ty'

            -- An error here indicates type/ kind checking has not been performed/ has
            -- been performed incorrectly as the expression in function
            -- position is not a big lambda.
            _ -> Left $ EMsg $ text "Can't reduce because the expression in function position of a big application isn't a big lambda"


  -- If the case scrutinee is an unbound variable, reduce all of the possible branches a step.
  -- , otherwise, find the first matching branch and bind all matching variables into the RHS before reducing it.
  --
  -- TODO: Add exhaustiveness checking/ checking for overlapping matches as
  -- currently this reduction may:
  -- - Fail to find a match.
  -- - Arbitrarily pick the first of an overlapping match.
  CaseAnalysis (Case caseScrutinee caseBranches)
    -> do reducedCaseScrutinee <- reduceStep ctx caseScrutinee
          case reducedCaseScrutinee of
            -- If the scrutinee is a binding that is unbound, we can still
            -- reduce each of the possible branches a step.
            Binding _
              -> (CaseAnalysis . Case reducedCaseScrutinee) <$> reducePossibleCaseRHSStep ctx caseBranches

            -- Otherwise we can proceed to identify a matching branch which we
            -- can substitute under like a lambda expression.
            _ -> reducePossibleCaseBranchesStep ctx reducedCaseScrutinee caseBranches

  _ -> error "Non-exhaustive pattern in reduction step"

-- | Reduce a step under the right hand side of each match.
--
-- This function is useful when the scrutinee of a case expression is unknown
-- (an unbound binding). If it is a known value, you may prefer to use
-- 'reducePossibleCaseBranchesStep' to identify and reduce only the matching
-- branch.
reducePossibleCaseRHSStep
  :: ReductionCtx
  -> CaseBranches Expr Pattern
  -> Either (Error Expr Type Pattern TypeCtx) (CaseBranches Expr Pattern)
reducePossibleCaseRHSStep ctx = sequenceCaseBranchesExpr . mapCaseBranchesExpr (reduceStep ctx)

-- | Given a case scrutinee that should _not_ be a Binding, find the matching
-- branch, substitute the expression and reduce a step.
--
-- If the case scrutinee is unknown (an unbound binding) you may want to use
-- 'reducePossibleCaseRHSStep' to reduce each possible branch.
reducePossibleCaseBranchesStep
  :: ReductionCtx
  -> Expr
  -> CaseBranches Expr Pattern
  -> Either (Error Expr Type Pattern TypeCtx) Expr
reducePossibleCaseBranchesStep ctx scrutineeExpr = \case
  -- With only a default branch there is no need to bind the matched value as it
  -- should be accessible in the outer scope.
  DefaultOnly defaultExpr
    -> reduceStep ctx defaultExpr

  -- With one-many branches and a possible default, we try each branch in order
  -- and fall back to the default if a match is not found.
  CaseBranches branches mDefaultExpr
    -> case tryBranches ctx scrutineeExpr branches of

         -- No branch matches. We therefore expect a default to exist.
         -- No branchs pattern matches with the expression. A default branch
         -- must exist or the case expression is invalid.
         Nothing
           -> case mDefaultExpr of
                -- TODO: Currently cases will work as long as they are only supplied matching
                -- values. Consider adding phase to require case-exhaustivity up front.
                Nothing
                  -> Left $ EMsg $ text "Case inexhaustive - No branch matches the expression and a default branch is not given"

                -- As with a DefaultOnly we don't bind the scrutineeExpression
                -- under the match body.
                Just defaultExpr
                  -> reduceStep ctx defaultExpr

         -- The branch matches, bind the scrutinee expression and reduce a step
         -- under the case body.
         Just (bindExprs,rhsExpr)
           -> reduceStep (underAppliedExpressions (reverse bindExprs) ctx) rhsExpr

-- | Try to match a (non-binding) expression against a collection of case
-- branches.
--
-- On success, return a list of all expressions to be bound under the selected
-- RHS body expression. Otherwise none of the branches have matched. When
-- multiple branches match, only the first is selected.
--
-- We assume the input is type-checked which ensures patterns have the same type as the casescrutinee matched
-- upon, and that all patterns are completly valid.
tryBranches
  :: ReductionCtx
  -> Expr
  -> NonEmpty (CaseBranch Expr Pattern)
  -> Maybe ([Expr],Expr)
tryBranches ctx caseScrutinee branches = firstMatch $ tryBranch ctx caseScrutinee <$> branches
  where
    -- The first 'Just'
    firstMatch :: NonEmpty (Maybe a) -> Maybe a
    firstMatch (h :| t) = safeHead . catMaybes $ h:t

    safeHead (x:xs) = Just x
    safeHead []     = Nothing

-- | Try and match a (non-binding) expression against a case branch.
--
-- On success, return a list of all expressions to be bound under the selected
-- RHS body expression.
--
-- We assume the input is type-checked which ensures patterns have the same type as the casescrutinee matched
-- upon, and that all patterns are completly valid.
tryBranch
  :: ReductionCtx
  -> Expr
  -> CaseBranch Expr Pattern
  -> Maybe ([Expr],Expr)
tryBranch ctx caseScrutinee (CaseBranch pattern rhsExpr) =
  (,rhsExpr) <$> patternBinding ctx caseScrutinee pattern

-- | Given a scrutinee expression and a pattern, if the pattern matches,
-- return the list of expressions that should be bound under the RHS.
patternBinding
  :: ReductionCtx
  -> Expr
  -> Pattern
  -> Maybe [Expr]
patternBinding ctx caseScrutinee pattern = case (caseScrutinee,pattern) of
  -- TODO: Switch to Either Error [Expr]

  -- This pattern matches when the scrutinee expression is equal to the
  -- expression referenced by a binding.
  (expr,BindingPattern b)
    -> case lookupVarBinding ctx b of
         Left err
           -> error . show $ err

         -- There is a difference between unknown if matching and not matching
         -- that is not being captured here

         -- The binding is unbound and so we don't know whether we've matched
         -- or not.
         -- TODO: The result type of this function should be extended to
         -- distinguish between match|no-match|unknown
         Right Unbound
           -> error "Checking matches with bindings that are unbound is not yet supported"

         -- If the expression is bound, there is a match if it reduces to
         -- exactly the same value.
         Right (Bound bExpr)
           -> case exprEq ctx expr bExpr of
                -- One of the expressions is invalid.
                -- This shouldnt happen because:
                -- - The expression should have been typechecked
                -- - The bound expressions should be valid.
                Left e
                  -> error $ show e

                -- Non match!
                Right False
                  -> Nothing

                -- Match! No patterns are allowed in regular expressions so there is nothing to bind.
                Right True
                  -> Just []

  -- Sum patterns begin matching when paired with a sum expression with the same
  -- index.
  (Sum sumExpr sumIx sumTys, SumPattern ix pattern)
    -- This index matches. Attempt to match further.
    | sumIx == ix
     -> patternBinding ctx sumExpr pattern

    -- Indexes are different.
    | otherwise
     -> Nothing

  -- Product patterns match when each constituent expression matches.
  (Product productExprs, ProductPattern patterns)
    -> patternBindings ctx productExprs patterns

  -- Union patterns begin matching when paired with a union expression with the
  -- same type index.
  (Union unionExpr unionTyIx unionTy, UnionPattern ty pattern)
    -- If the type matches attempt to match further.
    -- TODO: Consider using typeEq to determine matches. Currently types which
    -- would reduce to the same type do not match.
    | unionTyIx == ty
      -> patternBinding ctx unionExpr pattern

    -- Type indexes are different.
    | otherwise
      -> Nothing

  -- The entire expression is to be bound
  -- Bind matches whatever expression was supplied and binds it for the RHS.
  --
  -- TODO: At the top-level this functionality competes with Default branches
  -- and so we should consider removing one or the other.
  (expr,Bind)
    -> Just [expr]

  -- Either:
  -- - We've extended the Pattern type and missed a case - it's unfortunate GHC
  --   has exhaustiveness checking problems with patterns which make catching
  --   this harder.
  -- - Or we've been passed a scrutinee and Pattern pattern that have different
  --   types. This should have been caught by a type-checking phase.
  _ -> error "Failed to match an expression to a pattern. Either a type-mismatch has been missed or the implementation has forgotten to cover a case!"

-- Try matching all of these expressions against these patterns, return the list of bindings if successful.
--

-- | Require each pattern matches each expression in turn. If the patterns
-- match, return the list of expressions to be bound under the RHS.
--
-- Type/ validity checking should have ensured the input lists are the same
-- length and contain valid patterns.
patternBindings
  :: ReductionCtx
  -> [Expr]
  -> [Pattern]
  -> Maybe [Expr]
patternBindings ctx exprs patterns
  -- Sanity check that we have the same number of expressions and patterns
  | length exprs /= length patterns
   = error "Cannot decide whether a list of expressions and patterns match as there are differing amounts. This indicates the input has not been correctly type-checked."

  | otherwise
   = concat <$> zipWithM (patternBinding ctx) exprs patterns

-- | Test whether two expressions are equal under the same bindings.
-- Expressions are equal when they reduce to the same form as well as when they
-- are syntactically equal.
exprEq
  :: ReductionCtx
  -> Expr
  -> Expr
  -> Either (Error Expr Type Pattern TypeCtx) Bool
exprEq ctx e0 e1 = do
  redE0 <- reduce ctx e0
  redE1 <- reduce ctx e1
  Right $ redE0 == redE1

