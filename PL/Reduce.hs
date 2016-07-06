{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
module PL.Reduce where

import PL.Binds
import PL.Binds.Ix
import PL.Bindings
import PL.Case
import PL.Expr
import PL.Error
import PL.Type
import PL.Name

import Control.Applicative
import Control.Arrow (second)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy

import Data.Maybe

-- | Reduce an 'Expr'.
reduce :: forall b abs tb. (BindAbs b abs tb,Eq b) => Expr b abs tb -> Either (Error tb) (Expr b abs tb)
reduce expr = reduceRec emptyBindings expr
  where
  -- Recursively reduce an expression until it no longer reduces
  reduceRec :: Bindings (Expr b abs tb) -> Expr b abs tb -> Either (Error tb) (Expr b abs tb)
  reduceRec bindings expr = do
    redExpr <- reduceStep bindings expr
    if redExpr == expr then pure expr else reduceRec bindings redExpr

  reduceStep :: Bindings (Expr b abs tb) -> Expr b abs tb -> Either (Error tb) (Expr b abs tb)
  reduceStep bindings expr = case expr of

      -- Bindings reduce to whatever they've been bound to, if they've been bound that is.
      Binding b
        -> pure $ case index (Proxy :: Proxy b) bindings (bindDepth b) of
              Unbound -> Binding b
              Bound e -> e -- maybe should reduce again?

      -- Reduce the sums expression
      Sum sumExpr sumIx sumTy
        -> Sum <$> reduceStep bindings sumExpr <*> pure sumIx <*> pure sumTy

      -- Reduce all product expressions
      Product productExprs
        -> Product <$> mapM (reduceStep bindings) productExprs

      -- Reduce the unionExpr
      Union unionExpr unionTyIx unionTy
        -> Union <$> reduceStep bindings unionExpr <*> pure unionTyIx <*> pure unionTy

      -- Reduce under the lambda
      Lam takeTy lamExpr
        -> Lam <$> pure takeTy <*> reduceStep (unbound $ bury bindings) lamExpr

      -- 'strict'
      -- = reduce the argument, then the function, then the result of applying.
      App f x
        -> do x' <- reduceStep bindings x
              f' <- reduceStep bindings f
              case f' of
                Lam _ fExpr -> reduceStep (bind x' bindings) fExpr
                _           -> error "Cant reduce application of non-lambda term"

      -- If the case scrutinee is an unbound variable, reduce all of the possible branches
      -- , otherwise, find the first matching branch and bind all matching variables into the RHS before reducing it.
      CaseAnalysis (Case caseScrutinee caseBranches)
        -> do reducedCaseScrutinee <- reduceStep bindings caseScrutinee
              case reducedCaseScrutinee of
                Binding _ -> (CaseAnalysis . (Case reducedCaseScrutinee)) <$> reducePossibleCaseRHSs bindings caseBranches
                _         -> reducePossibleCaseBranches bindings reducedCaseScrutinee caseBranches


  -- The case scrutinee is not an unbound variable and so we can reduce the case expression, finding the matching branch and returning the reduced RHS.
  reducePossibleCaseBranches :: Bindings (Expr b abs tb) -> Expr b abs tb -> CaseBranches (Expr b abs tb) (MatchArg b tb) -> Either (Error tb) (Expr b abs tb)
  reducePossibleCaseBranches bindings caseExpr = \case
    DefaultOnly defaultExpr
      -> reduceStep bindings defaultExpr

    CaseBranches branches mDefaultExpr
      -> case tryBranches bindings caseExpr branches of

           -- No branch matches. We therefore expect a default to exist.
           Nothing
             -> case mDefaultExpr of
                  Nothing          -> Left $ EMsg "No branch matches the expression and a default branch is not given"
                  Just defaultExpr -> reduceStep bindings defaultExpr

           -- Branch matches, reduce its rhs under any bindings
           Just (bindExprs,rhsExpr)
             -> reduceStep (bindAll (reverse bindExprs) bindings) rhsExpr

  tryBranches :: Bindings (Expr b abs tb) -> Expr b abs tb -> NonEmpty (CaseBranch (Expr b abs tb) (MatchArg b tb)) -> Maybe ([Expr b abs tb],Expr b abs tb)
  tryBranches bindings caseScrutinee branches = firstMatch $ tryBranch bindings caseScrutinee <$> branches
    where
      -- The first 'Just'
      firstMatch :: NonEmpty (Maybe a) -> Maybe a
      firstMatch (h :| t) = safeHead . map fromJust . filter isJust $ h:t

      safeHead (x:xs) = Just x
      safeHead []     = Nothing

  -- Try and match a branch against an expression.
  -- On success return a list of all expressions to be bound under a RHS expression
  --
  -- We assume the input is type-checked which ensures patterns have the same type as the casescrutinee matched
  -- upon, and that all patterns are completly valid.
  tryBranch :: Bindings (Expr b abs tb) -> Expr b abs tb -> CaseBranch (Expr b abs tb) (MatchArg b tb) -> Maybe ([Expr b abs tb],Expr b abs tb)
  tryBranch bindings caseScrutinee (CaseBranch matchArg rhsExpr) = (,rhsExpr) <$> patternBinding bindings caseScrutinee matchArg

  -- Nothing on non match or a list of exprs to bind under the rhs.
  patternBinding :: Bindings (Expr b abs tb) -> Expr b abs tb -> MatchArg b tb -> Maybe [Expr b abs tb]
  patternBinding bindings caseScrutinee matchArg = case (caseScrutinee,matchArg) of

    (expr,MatchBinding b)
      -> case index (Proxy :: Proxy b) bindings (bindDepth b) of
             -- There is a difference between unknown if matching and not matching
             -- that is not being captured here
             Unbound     -> Nothing

             -- Do the two expressions reduce to exactly the same value?
             Bound bExpr -> do case exprEq bindings expr bExpr of

                                   -- One of the expressions is invalid.
                                   -- We're assuming this cant happen because:
                                   -- - The case expression should have been checked before now
                                   -- - All expressions in 'Bindings' should be checked as they are entered
                                   --
                                   -- - TODO: second point isnt strongly guaranteed by anything but how the current
                                   -- implementation happens to (hopefully) currently act.
                                   Left e -> error $ show e

                                   -- Non match!
                                   Right False -> Nothing

                                   -- Match! No patterns are allowed in regular expressions so nothing to bind
                                   Right True  -> Just []

    (Sum sumExpr sumIx sumTys, MatchSum ix matchArg)
      | sumIx == ix -> patternBinding bindings sumExpr matchArg -- matches this ix. Try match further
      | otherwise   -> Nothing                         -- matches another index

    --
    (Product productExprs, MatchProduct matchArgs)
      -> patternBindings bindings productExprs matchArgs -- curry isomorphism?

    (Union unionExpr unionTyIx unionTy, MatchUnion ty matchArg)
      -- TODO typeEq
      | unionTyIx == ty -> patternBinding bindings unionExpr matchArg -- matches this ty. Try match further.
      | otherwise       -> Nothing                           -- matches another type in the union

    -- The entire expression is to be bound
    (expr,Bind)
      -> Just [expr]

    _ -> error "Expression under case analysis has a different type to the match branch."

  -- Try matching all of these expressions against these patterns, return the list of bindings if successful.
  --
  -- (Type checking ensures same length lists and valid patterns)
  patternBindings :: Bindings (Expr b abs tb) -> [Expr b abs tb] -> [MatchArg b tb] -> Maybe [Expr b abs tb]
  patternBindings bindings exprs matchArgs = fmap concat $ mapM (uncurry $ patternBinding bindings) $ zip exprs matchArgs

  -- | Are two expressions identical under the same bindings?
  exprEq :: Bindings (Expr b abs tb) -> Expr b abs tb -> Expr b abs tb -> Either (Error tb) Bool
  exprEq bindings e0 e1 = do
    redE0 <- reduceRec bindings e0
    redE1 <- reduceRec bindings e1
    Right $ redE0 == redE1


  -- We've decided we cant evaluate the case expression yet, so we just reduce all the possible RHSs as much as possible
  reducePossibleCaseRHSs :: Bindings (Expr b abs tb)
                         -> CaseBranches (Expr b abs tb) (MatchArg b tb)
                         -> Either (Error tb) (CaseBranches (Expr b abs tb) (MatchArg b tb))
  reducePossibleCaseRHSs bindings = sequenceCaseBranchesExpr . mapCaseBranchesExpr (reduceStep bindings)

