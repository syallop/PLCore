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
module PL.Reduce where

import PL.Bindings
import PL.Binds
import PL.Binds.Ix
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Name
import PL.Type

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Proxy

import qualified Data.Text as Text

import PLPrinter

-- | Reduce an 'Expr'.
reduce
  :: forall b abs tb
   . ( BindAbs b abs tb
     , Eq b
     )
  => Expr b abs tb
  -> Either (Error tb) (Expr b abs tb)
reduce = reduceRec emptyBindings
  where
  -- Recursively reduce an expression until it no longer reduces
  reduceRec :: Bindings (Expr b abs tb) -> Expr b abs tb -> Either (Error tb) (Expr b abs tb)
  reduceRec bindings expr = do
    redExpr <- reduceStep bindings expr
    if redExpr == expr then pure expr else reduceRec bindings redExpr

  reduceStep :: Bindings (Expr b abs tb) -> Expr b abs tb -> Either (Error tb) (Expr b abs tb)
  reduceStep bindings expr = case unfixExpr expr of

      -- Bindings reduce to whatever they've been bound to, if they've been bound that is.
      Binding b
        -> let ix = bindDepth b
            in case safeIndex (Proxy :: Proxy b) bindings ix of
              Just Unbound
                -> pure $ fixExpr $ Binding b

              Just (Bound e)
                -> pure $ e -- maybe should reduce again?

              -- TODO: Is there a better stage to detect invalid bindings?
              Nothing
                -> Left $ EMsg $ text "Cannot bind an expression from " <> (text . Text.pack . show $ ix) <> text " abstractions away as there are not that many abstractions"

      -- Reduce the sums expression
      Sum sumExpr sumIx sumTy
        -> fmap fixExpr $ Sum <$> reduceStep bindings sumExpr <*> pure sumIx <*> pure sumTy

      -- Reduce all product expressions
      Product productExprs
        -> fmap fixExpr $ Product <$> mapM (reduceStep bindings) productExprs

      -- Reduce the unionExpr
      Union unionExpr unionTyIx unionTy
        -> fmap fixExpr $ Union <$> reduceStep bindings unionExpr <*> pure unionTyIx <*> pure unionTy

      -- Reduce under the lambda
      Lam takeTy lamExpr
        -> fmap fixExpr $ Lam <$> pure takeTy <*> reduceStep (unbound $ bury bindings) lamExpr

      -- 'strict'
      -- = reduce the argument, then the function, then the result of applying.
      App f x
        -> do x' <- reduceStep bindings x
              f' <- reduceStep bindings f
              case unfixExpr f' of
                Lam _ fExpr -> reduceStep (bind x' bindings) fExpr
                Binding var -> pure $ fixExpr $ App (fixExpr $ Binding var) x'
                _           -> Left $ EMsg $ text "Can't reduce because the expression in function position of an application isn't a lambda"

      -- Reduce under the Big Lambda/ App
      -- TODO: Pass kind bindings through the reduction as with Lam
      BigLam takeKind lamExpr
        -> Left $ EMsg $ text "Reducing under big lambdas is not implemented yet"
      BigApp f ty
        -> Left $ EMsg $ text "Reducing under big applications is not implemented yet"

      -- If the case scrutinee is an unbound variable, reduce all of the possible branches
      -- , otherwise, find the first matching branch and bind all matching variables into the RHS before reducing it.
      CaseAnalysis (Case caseScrutinee caseBranches)
        -> do reducedCaseScrutinee <- reduceStep bindings caseScrutinee
              case unfixExpr reducedCaseScrutinee of
                Binding _ -> fmap fixExpr $ (CaseAnalysis . Case reducedCaseScrutinee) <$> reducePossibleCaseRHSs bindings caseBranches
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
                  Nothing          -> Left $ EMsg $ text "No branch matches the expression and a default branch is not given"
                  Just defaultExpr -> reduceStep bindings defaultExpr

           -- Branch matches, reduce its rhs under any bindings
           Just (bindExprs,rhsExpr)
             -> reduceStep (bindAll (reverse bindExprs) bindings) rhsExpr

  tryBranches :: Bindings (Expr b abs tb) -> Expr b abs tb -> NonEmpty (CaseBranch (Expr b abs tb) (MatchArg b tb)) -> Maybe ([Expr b abs tb],Expr b abs tb)
  tryBranches bindings caseScrutinee branches = firstMatch $ tryBranch bindings caseScrutinee <$> branches
    where
      -- The first 'Just'
      firstMatch :: NonEmpty (Maybe a) -> Maybe a
      firstMatch (h :| t) = safeHead . catMaybes $ h:t

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
  patternBinding bindings caseScrutinee matchArg = case (unfixExpr caseScrutinee,matchArg) of

    (expr,MatchBinding b)
      -> case index (Proxy :: Proxy b) bindings (bindDepth b) of
             -- There is a difference between unknown if matching and not matching
             -- that is not being captured here
             Unbound     -> Nothing

             -- Do the two expressions reduce to exactly the same value?
             Bound bExpr -> case exprEq bindings (fixExpr expr) bExpr of

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
      -> Just [fixExpr expr]

    _ -> error "Expression under case analysis has a different type to the match branch."

  -- Try matching all of these expressions against these patterns, return the list of bindings if successful.
  --
  -- (Type checking ensures same length lists and valid patterns)
  patternBindings :: Bindings (Expr b abs tb) -> [Expr b abs tb] -> [MatchArg b tb] -> Maybe [Expr b abs tb]
  patternBindings bindings exprs matchArgs = concat <$> zipWithM (patternBinding bindings) exprs matchArgs

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

