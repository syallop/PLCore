{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
module PL.Reduce where

import PL.Binds
import PL.Bindings
import PL.Expr
import PL.Error
import PL.Type
import PL.Name

import Control.Applicative
import Control.Arrow (second)

import Data.Maybe

-- | Reduce an 'Expr'.
reduce :: forall b abs tb. BindAbs b abs tb => Expr b abs tb -> Either (Error tb) (Expr b abs tb)
reduce expr = reduceRec emptyBindings expr
  where
  -- Recursively reduce an expression until it no longer reduces
  reduceRec :: Bindings b abs tb -> Expr b abs tb -> Either (Error tb) (Expr b abs tb)
  reduceRec bindings expr = do
    redExpr <- reduceStep bindings expr
    if redExpr == expr then pure expr else reduceRec bindings redExpr

  reduceStep :: Bindings b abs tb -> Expr b abs tb -> Either (Error tb) (Expr b abs tb)
  reduceStep bindings expr = case expr of

      -- Bindings reduce to whatever they've been bound to, if they've been bound that is.
      Binding b
        -> pure $ case bindings `index` (bindDepth b) of
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

      -- If the caseExpression is a variable, reduce all of the possible branches
      -- , otherwise, find the first matching branch and bind all matching varibles into the RHS, then reduce it.
      Case caseExpr caseBranches
        -> do caseExpr' <- reduceStep bindings caseExpr
              case caseExpr' of
                 Binding _ -> Case <$> pure caseExpr' <*> reducePossibleCaseRHSs bindings caseBranches
                 _         -> reducePossibleCaseBranches bindings caseExpr' caseBranches


  -- The expression is not a variable and so we can reduce the case expession, finding the matching branch and returning the reduced RHS.
  reducePossibleCaseBranches :: Bindings b abs tb -> Expr b abs tb -> PossibleCaseBranches b abs tb -> Either (Error tb) (Expr b abs tb)
  reducePossibleCaseBranches bindings caseExpr = \case
      DefaultOnly defaultExpr
        -> reduceStep bindings defaultExpr

      CaseBranches branches mDefaultExpr
        -> case (tryBranches bindings caseExpr branches,mDefaultExpr) of

              -- No branch matches, so default should exist and be used.
              (Nothing,Just defaultExpr)
                 -> reduceStep bindings defaultExpr

              -- Branch matches, reduce its rhs under any bindings.
              (Just (bindExprs,rhsExpr),_)
                -> reduceStep (append (reverse bindExprs) bindings) rhsExpr

              -- No branch matches and a default has not been given.
              (Nothing,Nothing)
                -> Left $ EMsg "No branch matches expression and a default branch is not given"

  tryBranches :: Bindings b abs tb -> Expr b abs tb -> SomeCaseBranches b abs tb -> Maybe ([Expr b abs tb],Expr b abs tb)
  tryBranches bindings caseExpr (SomeCaseBranches caseBranch caseBranches) =
      firstMatch $ tryBranch bindings caseExpr <$> (caseBranch : caseBranches)

  firstMatch :: [Maybe a] -> Maybe a
  firstMatch = safeHead . map fromJust . filter isJust

  safeHead (x:xs) = Just x
  safeHead []     = Nothing

  -- Try and match a branch against an expression.
  -- On success return a list of all expressions to be bound under a RHS expression.
  --
  -- We assume the input is type-checked which ensures patterns have the same type as the caseexpr matched
  -- upon, and all patterns are completly valid.
  tryBranch :: Bindings b abs tb -> Expr b abs tb -> CaseBranch b abs tb -> Maybe ([Expr b abs tb],Expr b abs tb)
  tryBranch bindings caseExpr caseBranch =
      let CaseBranch matchArg rhsExpr = caseBranch
         in (,rhsExpr) <$> patternBinding bindings caseExpr matchArg

  -- Try matching all of these expressions against these patterns, return the list of bindings if successful.
  --
  -- (Type checking ensures same length lists and valid patterns)
  patternBindings :: Bindings b abs tb -> [Expr b abs tb] -> [MatchArg b tb] -> Maybe [Expr b abs tb]
  patternBindings bindings exprs matchArgs = fmap concat $ mapM (uncurry $ patternBinding bindings) $ zip exprs matchArgs

  -- This expression can be matched by this matchArgs. Return the list of bindings.
  patternBinding :: Bindings b abs tb -> Expr b abs tb -> MatchArg b tb -> Maybe [Expr b abs tb]
  patternBinding bindings expr matchArg = case (expr,matchArg) of

      (expr,MatchBinding b)
        -> case bindings `index` bindDepth b of
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

  -- | Are two expressions identical under the same bindings?
  exprEq :: Bindings b abs tb -> Expr b abs tb -> Expr b abs tb -> Either (Error tb) Bool
  exprEq bindings e0 e1 = do
    redE0 <- reduceRec bindings e0
    redE1 <- reduceRec bindings e1
    Right $ redE0 == redE1

  -- we cant evaluate the case expression yet, so we just reduce all the possible RHSs as much as possible.
  reducePossibleCaseRHSs :: Bindings b abs tb -> PossibleCaseBranches b abs tb -> Either (Error tb) (PossibleCaseBranches b abs tb)
  reducePossibleCaseRHSs bindings = \case
    DefaultOnly onMatchExpr
      -> DefaultOnly <$> reduceStep bindings onMatchExpr

    CaseBranches branches mDefaultExpr
      -> CaseBranches <$> (reduceSomeBranchRHSs bindings branches)
                      <*> maybe (pure Nothing) (Just <$>) (reduceStep bindings <$> mDefaultExpr)

  reduceSomeBranchRHSs :: Bindings b abs tb -> SomeCaseBranches b abs tb -> Either (Error tb) (SomeCaseBranches b abs tb)
  reduceSomeBranchRHSs bindings (SomeCaseBranches caseBranch caseBranches) =
    SomeCaseBranches <$> (reduceBranchRHS bindings caseBranch) <*> (mapM (reduceBranchRHS bindings) caseBranches)

  reduceBranchRHS :: Bindings b abs tb -> CaseBranch b abs tb -> Either (Error tb) (CaseBranch b abs tb)
  reduceBranchRHS bindings (CaseBranch lhs rhs) = CaseBranch <$> pure lhs <*> reduceStep bindings rhs

