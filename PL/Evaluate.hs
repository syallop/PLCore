{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , LambdaCase
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  #-}
{-|
Module      : PL.Evaluate
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Evaluate an expression by reducing it, subsituting ContentBindings and recursing
until the expression no longer changes.

This is currently very similar to reduction which behaves the same except it
does not substitute ContentBindings.

The input should have been reduced and type-checked and the implementation may
rely upon this being true.

Evaluation should eventually terminate as there are currently no recursive
references or self references allowed.

Evaluation has access to a CodeBase in which it may retrieve or store
expressions/ types/ kinds/ relations.

Functionality available at this level but not for reduction may one day include:
- IO actions
- Limited forms of recursion and self-reference
-}
module PL.Evaluate
  ( EvaluationCtx (..)
  , topEvaluationCtx

  , Evaluate ()
  , runEvaluate

  , evaluate
  , evaluateStep
  , evaluateType
  )
  where

-- PL
import PL.Bindings
import PL.Binds.Ix
import PL.Case
import PL.Store.Code
import PL.Error
import PL.Expr
import PL.FixPhase
import PL.Name
import PL.Pattern
import PL.ReduceType
import PL.Type
import PL.TypeCtx
import PL.Var

-- External PL
import PLPrinter

-- Other
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | EvaluationCtx contains information used/ generated when evaluating an
-- expression.
data EvaluationCtx = EvaluationCtx
  { _evaluationCodeStore    :: CodeStore

  , _evaluationExprBindings :: Bindings Expr
  , _evaluationTypeBindings :: Bindings Type

  , _evaluationSelfType     :: Maybe Type

  , _evaluationTypeCtx      :: TypeCtx
  , _evaluationGas          :: Maybe Int
  }

-- | Context for evaluating a top-level expression that has been:
-- - Type checked
-- - Reduced
topEvaluationCtx
  :: TypeCtx
  -> CodeStore
  -> EvaluationCtx
topEvaluationCtx typeCtx codeStore = EvaluationCtx
  { _evaluationCodeStore    = codeStore

  , _evaluationExprBindings = emptyBindings
  , _evaluationTypeBindings = emptyBindings

  , _evaluationSelfType     = Nothing

  , _evaluationTypeCtx      = typeCtx
  , _evaluationGas          = Nothing
  }

-- | Type of evaluation computations that:
--
-- - Have access to an EvaluationCtx as state
-- - May execute IO
-- - Have short-circuiting Errors
--
-- Run with runEvaluate.
-- Construct a context either from topEvaluationCtx or manually.
newtype Evaluate a = Evaluate {_evaluate :: EvaluationCtx -> IO (Either Error (EvaluationCtx,a))}

-- | Execute an Evaluate function to produce either the first error or the
-- result and the final context.
runEvaluate
  :: EvaluationCtx
  -> Evaluate a
  -> IO (Either Error (EvaluationCtx, a))
runEvaluate ctx e = _evaluate e ctx

-- Execute a function on the underlying CodeStore.
withCodeStore
  :: CodeStoreFunction a
  -> Evaluate a
withCodeStore f = Evaluate $ \ctx -> do
  eRes <- runCodeStoreFunction (_evaluationCodeStore ctx) f
  case eRes of
    Left err
      -> pure . Left $ err

    Right (codeStore, res)
      -> pure . Right $ (ctx{_evaluationCodeStore = codeStore}, res)

instance Functor Evaluate where
  fmap f (Evaluate e) = Evaluate $ \ctx -> do
    r <- e ctx
    case r of
      Left err
        -> pure $ Left err

      Right (ctx',a)
        -> pure $ Right (ctx', f a)

instance Applicative Evaluate where
  pure a = Evaluate $ \ctx -> pure $ Right (ctx, a)

  -- Perhaps this should be parallel on the initial context
  (Evaluate fab) <*> (Evaluate a) = Evaluate $ \ctx -> do
    r  <- fab ctx
    case r of
      Left err
        -> pure $ Left err
      Right (ctx', f)
        -> do a' <- a ctx'
              case a' of
                Left err
                  -> pure $ Left err
                Right (ctx'',a'')
                  -> pure $ Right (ctx'', f a'')

instance Monad Evaluate where
  return = pure

  (Evaluate fa) >>= fAToEvaluateB = Evaluate $ \ctx -> do
    r <- fa ctx
    case r of
      Left err
        -> pure $ Left err

      Right (ctx',a)
        -> let Evaluate fb = fAToEvaluateB a
            in fb ctx'

-- | True when the EvaluationCtx has been provided limited gas which has been
-- depleted.
hitEvaluationLimit
  :: Evaluate Bool
hitEvaluationLimit  = Evaluate $ \ctx -> pure $ Right (ctx, case _evaluationGas ctx of
                                                              Nothing
                                                                -> False
                                                              Just g
                                                                -> g <= 0
                                                      )

-- | If the amount of evaluation steps has a gas limit, reduce the amount available.
reduceEvaluationLimit
  :: Evaluate ()
reduceEvaluationLimit = Evaluate $ \ctx -> pure $ Right (ctx{_evaluationGas = fmap (subtract 1) $ _evaluationGas ctx},())

-- | Create a context for reducing types, built upon the current context for
-- building expressions.
--
-- This is what allows types at the expression level to be passed into types.
toTypeReductionCtx
  :: Evaluate (TypeReductionCtx DefaultPhase)
toTypeReductionCtx = Evaluate $ \ctx -> pure $ Right (ctx, TypeReductionCtx (_evaluationTypeBindings ctx) (_evaluationSelfType ctx) (_evaluationTypeCtx ctx) (_evaluationGas ctx))

-- | Query for the Bound Expr associated with a variable binding.
--
-- Fails when the variable supplied is too large or Unbound has been used - at
-- the evaluation phase we don't evaluate under abstractions and so all
-- variables we lookup should be bound.
lookupBoundVar
  :: Var
  -> Evaluate Expr
lookupBoundVar b = Evaluate $ \ctx -> pure $
  let ix       = bindDepth b
      bindings = _evaluationExprBindings ctx
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
          -> case binding of
               Unbound
                 -> Left . EMsg . mconcat $ [ text "Unbound variable in evaluation with index:"
                                            , lineBreak
                                            , indent1 . string . show $ ix
                                            ]
               Bound expr
                 -> Right (ctx, expr)

-- | Query the underlying CodeStore for the expression associated with a name.
lookupContentBinding
  :: ContentName
  -> Evaluate Expr
lookupContentBinding c = do
  mExpr <- withCodeStore $ lookupExpr (contentName c)
  case mExpr of
    Nothing
     -> evaluationFail . EMsg . text $ "Unknown expression. All expressions must be known before the evaluation phase."

    Just expr
     -> pure expr

-- | Context after an Expr is applied.
underAppliedExpression
  :: Expr
  -> Evaluate ()
underAppliedExpression appliedExpr = Evaluate $ \ctx -> pure $ Right $ (ctx{_evaluationExprBindings = bind appliedExpr . _evaluationExprBindings $ ctx},())

-- | Context after a many Expr are applied.
-- left-to-right.
-- (I.E. right-most becomes the nearest binding).
underAppliedExpressions
  :: [Expr]
  -> Evaluate ()
underAppliedExpressions appliedExprs = Evaluate $ \ctx -> pure $ Right $ (ctx{_evaluationExprBindings = bindAll appliedExprs . _evaluationExprBindings $ ctx},())

-- | Context after an Type is applied.
underAppliedType
  :: Type
  -> Evaluate ()
underAppliedType appliedType = Evaluate $ \ctx -> pure $ Right $ (ctx{_evaluationTypeBindings = bind appliedType . _evaluationTypeBindings $ ctx},())

-- | Fail with a given Error
evaluationFail
  :: Error
  -> Evaluate a
evaluationFail err = Evaluate $ \_ctx -> pure $ Left err

-- | Evaluate an expression by reducing it, binding variables and substituting ContentBindings from
-- the CodeStore until the expression no longer changes.
--
-- Unlike reduction, evaluation:
-- - Does not evaluate under abstractions (such as lambdas and type lambdas)
-- - _Does_ evaluate under ContentBindings which must be available.
--
-- Evaluate assumes the input expression is well-typed and it's dependencies are
-- available.
--
-- Evaluate should eventually terminate given enough gas as there are currently
-- no recursive references allowed in the AST, nor are there self-references.
evaluate
  :: Expr
  -> Evaluate Expr
evaluate initialExpr = do
  hitLimit <- hitEvaluationLimit
  if hitLimit
    then evaluationFail $ EMsg . mconcat $
       [ text "Evaluation limit reached when evaluating expression. Got: "
       , text . Text.pack . show $ initialExpr
       ]
    else do -- Apply the evaluation step until the expression no longer changes.
            -- This requires that evaluation eventually converges - diverging will lead to
            -- non-termination.
            --
            -- Bouncing back and forth is inefficient but makes it easier to:
            -- - Guard against non-termination
            -- - Generate traces of evaluation
            -- TODO:
            -- - We could probably have the best of both worlds by
            --   integrating the checks into the Evaluate type.
            -- - Alternatively we could trampoline better with continuations.
            evaluatedExpr <- evaluateStep initialExpr
            if evaluatedExpr == initialExpr
              then pure initialExpr
              else reduceEvaluationLimit >> evaluate evaluatedExpr

-- | Evaluate a single step under an expression where possible.
evaluateStep
  :: Expr
  -> Evaluate Expr
evaluateStep initialExpr = case initialExpr of
  -- Evaluation behaves differently to reduction and:
  -- - Does not stop at ContentBindings - but instead looks them up
  -- - Does not evaluate under abstractions (such as lambdas and biglambdas).
  --
  -- TODO:
  -- - Is this sensible?
  -- - Does evaluating `@ (\Bool \Bool 1) True` produce `\Bool 0` or does it
  --   produce `\Bool 1` under the context that `True` has been burried?
  --   If so, the resulting expression is meaningless without the final context
  --   and either:
  --   - Requires adjusting after all.
  --   - Implies we _should_ evaluate under abstractions

  -- We don't evaluate under abstractions and so bindings should always be
  -- bound.
  Binding b
    -> lookupBoundVar b

  -- Lookup content-binding in codebase and substitute.
  ContentBinding c
    -> lookupContentBinding c

  Sum sumExpr sumIx sumTy
    -> Sum <$> evaluateStep sumExpr <*> pure sumIx <*> mapM evaluateType sumTy

  Product productExprs
    -> Product <$> mapM evaluateStep productExprs

  Union unionExpr unionTyIx unionTys
    -> Union <$> evaluateStep unionExpr
             <*> evaluateType unionTyIx
             <*> ((fmap Set.fromList) <$> mapM evaluateType . Set.toList $ unionTys)

  -- Don't evaluate under lambdas. Require an argument first!
  Lam takeTy lamExpr
    -> Lam <$> evaluateType takeTy
           <*> pure lamExpr

  -- Evaluate using strictish semantics:
  -- - First fully evaluate the argument
  -- - Then fully evaluate the function
  -- - Then bind the expression and evaluate the entire expression a step.
  --
  -- Typechecking _should_ have ensured the function is an arrow type that
  -- matches the expression. It should therefore only be a lambda or a binding
  -- to a (possibly unknown) function.
  App f x
    -> do x' <- evaluate x
          f' <- evaluate f
          case f' of
            -- Lambdas are reduced by binding applied values into the body and
            -- continuing evaluation.
            Lam _abs fBodyExpr
              -> underAppliedExpression x' >> evaluateStep fBodyExpr

            -- The function we're applying is 'higher order' - it is sourced
            -- from a function itself.
            --
            -- If an expression was bound it should have been substituted in the
            -- function reduction and so we can assume it is unbound and do
            -- nothing except reduce the argument a step.
            -- If we're wrong an additional evaluationStep on the
            -- application should make progress.
            Binding var
              -> pure $ App (Binding var) x'
            ContentBinding c
              -> pure $ App (ContentBinding c) x'

            -- An error here indicates type checking has not been performed/ has
            -- been performed incorrectly as the expression in function
            -- position is not a lambda.
            _ -> evaluationFail $ EMsg $ text "Can't evaluate because the expression in function position of an application isn't a lambda"

  -- Do not reduce under big lambdas. Require an argument!
  BigLam takeKind lamExpr
    -> BigLam <$> pure takeKind
              <*> pure lamExpr

  -- Evaluate using strictish semantics:
  -- - First fully evaluate the applied type.
  -- - Then fully evaluate the function.
  -- - Then bind the type in the expression body and evaluate a step.
  --
  -- Type/ kind checking _should_ have ensured the function is a BigArrow type
  -- that matches the type. It should therefore only be a BigLambda or a
  -- typebinding to a (possibly) unknown function.
  BigApp f t
    -> do t' <- evaluateType t
          f' <- evaluate f
          case f' of
            -- Big Lambdas are reduced by binding applied types into the body
            -- and reducing.
            BigLam _absKind fBodyExpr
              -> underAppliedType t' >> evaluateStep fBodyExpr

            -- The function we're applying is 'higher order' - it is sourced
            -- from a function itself.
            --
            -- If a type was bound it should have been substituted in the
            -- function reduction and so we can assume it is unbound and do
            -- nothing except reduce the argument a step.
            -- If we're wrong an additional evaluateStep on the
            -- application should make progress.
            Binding var
              -> pure $ BigApp (Binding var) t'

            -- An error here indicates type/ kind checking has not been performed/ has
            -- been performed incorrectly as the expression in function
            -- position is not a big lambda.
            _ -> evaluationFail $ EMsg $ text "Can't evaluate because the expression in function position of a big application isn't a big lambda"

  -- Find the first matching branch and bind all matching variables into the RHS
  -- before evaluating it.
  --
  -- We assume the expression has been checked for exhaustiveness/ overlapping
  -- matches or this evaluation may:
  -- - Fail to find a match.
  -- - Arbitrarily pick the first of an overlapping match.
  CaseAnalysis (Case caseScrutinee caseBranches)
    -> do evaluatedCaseScrutinee <- evaluate caseScrutinee
          case evaluatedCaseScrutinee of
            -- This indicates the evaluation algorithm has peeked under an
            -- abstraction.
            Binding _
              -> evaluationFail $ EMsg $ text "Can't evaluate because a case scrutinee is an Unbound variable."

            -- Otherwise we can proceed to identify a matching branch which we
            -- can substitute under like a lambda expression.
            _ -> evaluatePossibleCaseBranchesStep evaluatedCaseScrutinee caseBranches

  _ -> evaluationFail $ EMsg $ text "Non-exhaustive pattern in evaluation step"

-- | Types may need to be evaluated when they capture types from the expression
-- level that have only been applied at the evaluation phase.
--
-- Internally this calls reduceType.
evaluateType
  :: Type
  -> Evaluate Type
evaluateType t = do
  ctx <- toTypeReductionCtx
  case reduceType ctx t of
    Left err
      -> evaluationFail err

    Right t'
      -> pure t'

-- | Given a case scrutinee that should _not_ be a Binding, find the matching
-- branch, substitute the expression and evaluate a step.
evaluatePossibleCaseBranchesStep
  :: Expr
  -> CaseBranches Expr Pattern
  -> Evaluate Expr
evaluatePossibleCaseBranchesStep scrutineeExpr = \case
  -- With only a default branch there is no need to bind the matched value as it
  -- should be accessible in the outer scope.
  DefaultOnly defaultExpr
    -> evaluateStep defaultExpr

  -- With one-many branches and a possible default, we try each branch in order
  -- and fall back to the default if a match is not found.
  CaseBranches branches mDefaultExpr
    -> do mMatch <- tryBranches scrutineeExpr branches
          case mMatch of

            -- No branch matches. We therefore expect a default to exist.
            -- No branchs pattern matches with the expression. A default branch
            -- must exist or the case expression is invalid.
            Nothing
              -> case mDefaultExpr of
                   -- TODO: Currently cases will work as long as they are only supplied matching
                   -- values. Consider adding phase to require case-exhaustivity up front.
                   Nothing
                     -> evaluationFail $ EMsg $ text "Case inexhaustive - No branch matches the expression and a default branch is not given"

                   -- As with a DefaultOnly we don't bind the scrutineeExpression
                   -- under the match body.
                   Just defaultExpr
                     -> evaluateStep defaultExpr

            -- The branch matches, bind the scrutinee expression and reduce a step
            -- under the case body.
            Just (bindExprs,rhsExpr)
              -> underAppliedExpressions (reverse bindExprs) >> evaluateStep rhsExpr

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
  :: Expr
  -> NonEmpty (CaseBranch Expr Pattern)
  -> Evaluate (Maybe ([Expr],Expr))
tryBranches caseScrutinee branches = do
  m <- mapM (tryBranch caseScrutinee) branches
  pure $ firstMatch m
  where
    -- The first 'Just'
    firstMatch :: NonEmpty (Maybe a) -> Maybe a
    firstMatch (h :| t) = safeHead . catMaybes $ h:t

    safeHead (x:_) = Just x
    safeHead []    = Nothing

-- | Try and match a (non-binding) expression against a case branch.
--
-- On success, return a list of all expressions to be bound under the selected
-- RHS body expression.
--
-- We assume the input is type-checked which ensures patterns have the same type as the casescrutinee matched
-- upon, and that all patterns are completly valid.
tryBranch
  :: Expr
  -> CaseBranch Expr Pattern
  -> Evaluate (Maybe ([Expr],Expr))
tryBranch caseScrutinee (CaseBranch pattern rhsExpr) = do
 toBind <- patternBinding caseScrutinee pattern
 pure $ ((,rhsExpr) <$> toBind)

-- | Given a scrutinee expression and a pattern, if the pattern matches,
-- return the list of expressions that should be bound under the RHS.
patternBinding
  :: Expr
  -> Pattern
  -> Evaluate (Maybe [Expr])
patternBinding caseScrutinee pattern = case (caseScrutinee,pattern) of
  -- TODO: Most of these functions could be shared with reduce

  -- This pattern matches when the scrutinee expression is equal to the
  -- expression referenced by a binding.
  (expr,BindingPattern b)
    -> do boundExpr <- lookupBoundVar b
          eq <- exprEq expr boundExpr
          if eq
            -- Match! No patterns are allowed in regular expressions so there is nothing to bind.
            then pure $ Just []
            else pure Nothing

  -- Sum patterns begin matching when paired with a sum expression with the same
  -- index.
  (Sum sumExpr sumIx _sumTys, SumPattern ix sumPattern)
    -- This index matches. Attempt to match further.
    | sumIx == ix
     -> patternBinding sumExpr sumPattern

    -- Indexes are different.
    | otherwise
     -> pure Nothing

  -- Product patterns match when each constituent expression matches.
  (Product productExprs, ProductPattern patterns)
    -> patternBindings productExprs patterns

  -- Union patterns begin matching when paired with a union expression with the
  -- same type index.
  (Union unionExpr unionTyIx _unionTy, UnionPattern t unionPattern)
    -- If the type matches attempt to match further.
    -- TODO: Consider using typeEq to determine matches. Currently types which
    -- would reduce to the same type do not match.
    | unionTyIx == t
      -> patternBinding unionExpr unionPattern

    -- Type indexes are different.
    | otherwise
      -> pure Nothing

  -- The entire expression is to be bound
  -- Bind matches whatever expression was supplied and binds it for the RHS.
  --
  -- TODO: At the top-level this functionality competes with Default branches
  -- and so we should consider removing one or the other.
  (expr,Bind)
    -> pure $ Just [expr]

  -- Either:
  -- - We've extended the Pattern type and missed a case - it's unfortunate GHC
  --   has exhaustiveness checking problems with patterns which make catching
  --   this harder.
  -- - Or we've been passed a scrutinee and Pattern pattern that have different
  --   types. This should have been caught by a type-checking phase.
  _ -> error "Failed to match an expression to a pattern. Either a type-mismatch has been missed or the implementation has forgotten to cover a case!"

-- | Require each pattern matches each expression in turn. If the patterns
-- match, return the list of expressions to be bound under the RHS.
--
-- Type/ validity checking should have ensured the input lists are the same
-- length and contain valid patterns.
patternBindings
  :: [Expr]
  -> [Pattern]
  -> Evaluate (Maybe [Expr])
patternBindings exprs patterns
  -- Sanity check that we have the same number of expressions and patterns
  | length exprs /= length patterns
   = error "Cannot decide whether a list of expressions and patterns match as there are differing amounts. This indicates the input has not been correctly type-checked."

  | otherwise
   = do toBind <- zipWithM patternBinding exprs patterns
        pure $ foldr (\mBind mAcc
                        -> do a <- mAcc
                              b <- mBind
                              Just (b <> a)
                      ) (Just []) toBind

-- | Test whether two expressions are equal under the same bindings.
-- Expressions are equal when they evaluate to the same form as well as when they
-- are syntactically equal.
exprEq
  :: Expr
  -> Expr
  -> Evaluate Bool
exprEq e0 e1 = do
  evalE0 <- evaluate e0
  evalE1 <- evaluate e1
  pure $ evalE0 == evalE1

