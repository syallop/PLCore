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
  ( reduce
  , reduceWith
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
  :: TypeCtx DefaultPhase
  -> Expr
  -> Either (Error Expr Type Pattern) Expr
reduce typeCtx expr = reduceWith emptyBindings emptyBindings typeCtx (Just 100) expr

-- | 'reduce' with a collection of initial bindings as if Expressions and Types
-- have already been applied to an outer lambda abstraction.
reduceWith
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Maybe Int
  -> Expr
  -> Either (Error Expr Type Pattern) Expr
reduceWith bindings typeBindings typeCtx reductionLimit initialExpr
  | reductionLimit == Just 0
   = Left . EMsg . mconcat $
       [ text "Reduction limit reached when reducing expression. Got: "
       , text . Text.pack . show $ initialExpr
       ]

  | otherwise
   = do -- Apply the reduce step until the expression no longer changes.
        -- This requires that reduction eventually converges - diverging will lead to
        -- non-termination.
        -- TODO: We could add a reduction limit here to guard against accidental
        -- diversion.
        -- TODO: reduceStep only reduces one level under expressions for each call. If
        -- we assume the expression reduces, we should reduce faster by recursing with
        -- reduceWith instead.
        reducedExpr <- reduceStep bindings typeBindings typeCtx initialExpr
        if reducedExpr == initialExpr
          then pure initialExpr
          else reduceWith bindings typeBindings typeCtx (fmap (subtract 1) reductionLimit) reducedExpr

-- | 'reduce' a single step with a collection of initial bindings as if Expressions and Types
-- have already been applied to an outer lambda abstraction.
reduceStep
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Expr
  -> Either (Error Expr Type Pattern) Expr
reduceStep bindings typeBindings typeCtx initialExpr = case initialExpr of
  -- TODO: Consider whether/ where we should reduce types.

  -- Bindings are substituted if they have been bound.
  Binding b
    -> let ix = bindDepth b
        in case safeIndex (Proxy :: Proxy Var) bindings ix of
          -- If no expression has been bound there is no further reduction
          -- possible.
          -- E.G. We're looking at '0' in a lambda that has not been applied: '\Foo 0`
          Just Unbound
            -> pure $ Binding b

          -- If an expression has been bound, we substitute it for the binding.
          -- We assume strict evaluation of the expression and so don't reduce
          -- it again.
          Just (Bound e')
            -> pure e'

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
            -> Left . EContext (EMsg . text $ "Reducing expression") . EBindExprLookupFailure ix $ bindings

  -- Reduce a single step under the sum expression.
  Sum sumExpr sumIx sumTy
    -> Sum <$> reduceStep bindings typeBindings typeCtx sumExpr
           <*> pure sumIx
           <*> mapM (reduceTypeWith typeBindings typeCtx (Just 100)) sumTy

  -- Reduce a single step under all product expressions.
  Product productExprs
    -> Product <$> mapM (reduceStep bindings typeBindings typeCtx) productExprs

  -- Reduce a single step under the union expression.
  Union unionExpr unionTyIx unionTys
    -> Union <$> reduceStep bindings typeBindings typeCtx unionExpr
             <*> reduceTypeWith typeBindings typeCtx (Just 100) unionTyIx
             <*> ((fmap Set.fromList) <$> mapM (reduceTypeWith typeBindings typeCtx (Just 100)) . Set.toList $ unionTys)

  -- Reduce a single step under the lambda expression.
  Lam takeTy lamExpr
    -> Lam <$> reduceTypeWith typeBindings typeCtx (Just 100) takeTy
           <*> reduceStep (unbound $ bury bindings) typeBindings typeCtx lamExpr

  -- Reduce using strictish semantics:
  -- - First reduce a step under the applied expression
  -- - Then reduce a step under the function
  -- - Then bind the expression and reduce the entire expression a step.
  --
  -- Typechecking _should_ have ensured the function is an arrow type that
  -- matches the expression. It should therefore only be a lambda or a binding
  -- to a (possibly unknown) function.
  App f x
    -> do x' <- reduceStep bindings typeBindings typeCtx x
          f' <- reduceStep bindings typeBindings typeCtx f
          case f' of
            -- Lambdas are reduced by binding applied values into the body and
            -- reducing.
            Lam _abs fBodyExpr
              -> reduceStep (bind x' bindings) typeBindings typeCtx fBodyExpr

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
    -> BigLam <$> pure takeKind <*> reduceStep bindings (unbound $ bury typeBindings) typeCtx lamExpr

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
          ty' <- reduceTypeWith typeBindings typeCtx (Just 100) ty
          f'  <- reduceStep bindings typeBindings typeCtx f
          case f' of
            -- Big Lambdas are reduced by binding applied types into the body
            -- and reducing.
            BigLam _absKind fBodyExpr
              -> reduceStep bindings (bind ty' typeBindings) typeCtx fBodyExpr

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
    -> do reducedCaseScrutinee <- reduceStep bindings typeBindings typeCtx caseScrutinee
          case reducedCaseScrutinee of
            -- If the scrutinee is a binding that is unbound, we can still
            -- reduce each of the possible branches a step.
            Binding _
              -> (CaseAnalysis . Case reducedCaseScrutinee) <$> reducePossibleCaseRHSStep bindings typeBindings typeCtx caseBranches

            -- Otherwise we can proceed to identify a matching branch which we
            -- can substitute under like a lambda expression.
            _ -> reducePossibleCaseBranchesStep bindings typeBindings typeCtx reducedCaseScrutinee caseBranches

  _ -> error "Non-exhaustive pattern in reduction step"

-- | Reduce a step under the right hand side of each match.
--
-- This function is useful when the scrutinee of a case expression is unknown
-- (an unbound binding). If it is a known value, you may prefer to use
-- 'reducePossibleCaseBranchesStep' to identify and reduce only the matching
-- branch.
reducePossibleCaseRHSStep
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> CaseBranches Expr Pattern
  -> Either (Error Expr Type Pattern) (CaseBranches Expr Pattern)
reducePossibleCaseRHSStep bindings typeBindings typeCtx = sequenceCaseBranchesExpr . mapCaseBranchesExpr (reduceStep bindings typeBindings typeCtx)

-- | Given a case scrutinee that should _not_ be a Binding, find the matching
-- branch, substitute the expression and reduce a step.
--
-- If the case scrutinee is unknown (an unbound binding) you may want to use
-- 'reducePossibleCaseRHSStep' to reduce each possible branch.
reducePossibleCaseBranchesStep
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Expr
  -> CaseBranches Expr Pattern
  -> Either (Error Expr Type Pattern) Expr
reducePossibleCaseBranchesStep bindings typeBindings typeCtx scrutineeExpr = \case
  -- With only a default branch there is no need to bind the matched value as it
  -- should be accessible in the outer scope.
  DefaultOnly defaultExpr
    -> reduceStep bindings typeBindings typeCtx defaultExpr

  -- With one-many branches and a possible default, we try each branch in order
  -- and fall back to the default if a match is not found.
  CaseBranches branches mDefaultExpr
    -> case tryBranches bindings typeBindings typeCtx scrutineeExpr branches of

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
                  -> reduceStep bindings typeBindings typeCtx defaultExpr

         -- The branch matches, bind the scrutinee expression and reduce a step
         -- under the case body.
         Just (bindExprs,rhsExpr)
           -> reduceStep (bindAll (reverse bindExprs) bindings) typeBindings typeCtx rhsExpr

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
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Expr
  -> NonEmpty (CaseBranch Expr Pattern)
  -> Maybe ([Expr],Expr)
tryBranches bindings typeBindings typeCtx caseScrutinee branches = firstMatch $ tryBranch bindings typeBindings typeCtx caseScrutinee <$> branches
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
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Expr
  -> CaseBranch Expr Pattern
  -> Maybe ([Expr],Expr)
tryBranch bindings typeBindings typeCtx caseScrutinee (CaseBranch pattern rhsExpr) =
  (,rhsExpr) <$> patternBinding bindings typeBindings typeCtx caseScrutinee pattern

-- | Given a scrutinee expression and a pattern, if the pattern matches,
-- return the list of expressions that should be bound under the RHS.
patternBinding
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Expr
  -> Pattern
  -> Maybe [Expr]
patternBinding bindings typeBindings typeCtx caseScrutinee pattern = case (caseScrutinee,pattern) of

  -- This pattern matches when the scrutinee expression is equal to the
  -- expression referenced by a binding.
  (expr,BindingPattern b)
    -> case index (Proxy :: Proxy Var) bindings (bindDepth b) of
           -- There is a difference between unknown if matching and not matching
           -- that is not being captured here

           -- The binding is unbound and so we don't know whether we've matched
           -- or not.
           -- TODO: The result type of this function should be extended to
           -- distinguish between match|no-match|unknown
           Unbound
             -> error "Checking matches with bindings that are unbound is not yet supported."

           -- If the expression is bound, there is a match if it reduces to
           -- exactly the same value.
           Bound bExpr
             -> case exprEq bindings typeBindings typeCtx expr bExpr of

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
     -> patternBinding bindings typeBindings typeCtx sumExpr pattern

    -- Indexes are different.
    | otherwise
     -> Nothing

  -- Product patterns match when each constituent expression matches.
  (Product productExprs, ProductPattern patterns)
    -> patternBindings bindings typeBindings typeCtx productExprs patterns

  -- Union patterns begin matching when paired with a union expression with the
  -- same type index.
  (Union unionExpr unionTyIx unionTy, UnionPattern ty pattern)
    -- If the type matches attempt to match further.
    -- TODO: Consider using typeEq to determine matches. Currently types which
    -- would reduce to the same type do not match.
    | unionTyIx == ty
      -> patternBinding bindings typeBindings typeCtx unionExpr pattern

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
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> [Expr]
  -> [Pattern]
  -> Maybe [Expr]
patternBindings bindings typeBindings typeCtx exprs patterns
  -- Sanity check that we have the same number of expressions and patterns
  | length exprs /= length patterns
   = error "Cannot decide whether a list of expressions and patterns match as there are differing amounts. This indicates the input has not been correctly type-checked."

  | otherwise
   = concat <$> zipWithM (patternBinding bindings typeBindings typeCtx) exprs patterns

-- | Test whether two expressions are equal under the same bindings.
-- Expressions are equal when they reduce to the same form as well as when they
-- are syntactically equal.
exprEq
  :: Bindings Expr
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Expr
  -> Expr
  -> Either (Error Expr Type Pattern) Bool
exprEq bindings typeBindings typeCtx e0 e1 = do
  redE0 <- reduceWith bindings typeBindings typeCtx (Just 100) e0
  redE1 <- reduceWith bindings typeBindings typeCtx (Just 100) e1
  Right $ redE0 == redE1

