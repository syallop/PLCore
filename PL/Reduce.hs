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

-- | Within a 'NameCtx', reduce an 'Expr'.
reduce :: forall b abs. BindAbs b abs => NameCtx -> Expr b abs -> Either Error (Expr b abs)
reduce nameCtx expr = reduceRec emptyBindings nameCtx expr
  where
  -- Recursively reduce an expression until it no longer reduces
  reduceRec :: Bindings b abs -> NameCtx -> Expr b abs -> Either Error (Expr b abs)
  reduceRec bindings nameCtx expr = do
    redExpr <- reduceStep bindings nameCtx expr
    if redExpr == expr then pure expr else reduceRec bindings nameCtx redExpr

  reduceStep :: Bindings b abs -> NameCtx -> Expr b abs -> Either Error (Expr b abs)
  reduceStep bindings nameCtx expr = case expr of

      -- Terms dont reduce
      Term termName
        -> Right $ Term termName

      -- Bindings reduce to whatever they've been bound to, if they've been bound that is.
      Binding b
        -> pure $ case bindings `index` (bindDepth b) of
              Unbound -> Binding b
              Bound e -> e -- maybe should reduce again?

      -- Reduce the sums expression
      Sum sumExpr sumIx sumTy
        -> Sum <$> reduceStep bindings nameCtx sumExpr <*> pure sumIx <*> pure sumTy

      -- Reduce all product expressions
      Product productExprs
        -> Product <$> mapM (reduceStep bindings nameCtx) productExprs

      -- Reduce the unionExpr
      Union unionExpr unionTyIx unionTy
        -> Union <$> reduceStep bindings nameCtx unionExpr <*> pure unionTyIx <*> pure unionTy

      -- Reduce under the lambda
      Lam takeTy lamExpr
        -> Lam <$> pure takeTy <*> reduceStep (unbound $ bury bindings) nameCtx lamExpr

      -- 'strict'
      -- = reduce the argument, then the function, then the result of applying.
      App f x
        -> do x' <- reduceStep bindings nameCtx x
              f' <- reduceStep bindings nameCtx f
              case f' of
                Lam _ fExpr -> reduceStep (bind x' bindings) nameCtx fExpr
                Term tName  -> Right $ App (Term tName) x'
                _           -> error "Cant reduce application of non-lambda term"

      -- If the caseExpression is a variable, reduce all of the possible branches
      -- , otherwise, find the first matching branch and bind all matching varibles into the RHS, then reduce it.
      Case caseExpr caseBranches
        -> do caseExpr' <- reduceStep bindings nameCtx caseExpr
              case caseExpr' of
                 Binding _ -> Case <$> pure caseExpr' <*> reducePossibleCaseRHSs bindings nameCtx caseBranches
                 _         -> reducePossibleCaseBranches bindings nameCtx caseExpr' caseBranches


  -- The expression is not a variable and so we can reduce the case expession, finding the matching branch and returning the reduced RHS.
  reducePossibleCaseBranches :: Bindings b abs -> NameCtx -> Expr b abs -> PossibleCaseBranches b abs -> Either Error (Expr b abs)
  reducePossibleCaseBranches bindings nameCtx caseExpr = \case
      DefaultOnly defaultExpr
        -> reduceStep bindings nameCtx defaultExpr

      CaseBranches branches mDefaultExpr
        -> case (tryBranches bindings nameCtx caseExpr branches,mDefaultExpr) of

              -- No branch matches, so default should exist and be used.
              (Nothing,Just defaultExpr)
                 -> reduceStep bindings nameCtx defaultExpr

              -- Branch matches, reduce its rhs under any bindings.
              (Just (bindExprs,rhsExpr),_)
                -> reduceStep (append (reverse bindExprs) bindings) nameCtx rhsExpr

  tryBranches :: Bindings b abs -> NameCtx -> Expr b abs -> SomeCaseBranches b abs -> Maybe ([Expr b abs],Expr b abs)
  tryBranches bindings nameCtx caseExpr (SomeCaseBranches caseBranch caseBranches) =
      firstMatch $ tryBranch bindings nameCtx caseExpr <$> (caseBranch : caseBranches)

  firstMatch :: [Maybe a] -> Maybe a
  firstMatch = safeHead . map fromJust . filter isJust

  safeHead (x:xs) = Just x
  safeHead []     = Nothing

  -- Try and match a branch against an expression.
  -- On success return a list of all expressions to be bound under a RHS expression.
  --
  -- We assume the input is type-checked which ensures patterns have the same type as the caseexpr matched
  -- upon, and all patterns are completly valid.
  tryBranch :: Bindings b abs -> NameCtx -> Expr b abs -> CaseBranch b abs -> Maybe ([Expr b abs],Expr b abs)
  tryBranch bindings nameCtx caseExpr caseBranch =
      let CaseBranch matchArg rhsExpr = caseBranch
         in (,rhsExpr) <$> patternBinding bindings nameCtx caseExpr matchArg

  -- Try matching all of these expressions against these patterns, return the list of bindings if successful.
  --
  -- (Type checking ensures same length lists and valid patterns)
  patternBindings :: Bindings b abs -> NameCtx -> [Expr b abs] -> [MatchArg b] -> Maybe [Expr b abs]
  patternBindings bindings nameCtx exprs matchArgs = fmap concat $ mapM (uncurry $ patternBinding bindings nameCtx) $ zip exprs matchArgs

  -- This expression can be matched by this matchArgs. Return the list of bindings.
  patternBinding :: Bindings b abs -> NameCtx -> Expr b abs -> MatchArg b -> Maybe [Expr b abs]
  patternBinding bindings nameCtx expr matchArg = case (expr,matchArg) of

      (expr,MatchBinding b)
        -> case bindings `index` bindDepth b of
               -- There is a difference between unknown if matching and not matching
               -- that is not being captured here
               Unbound     -> Nothing

               -- Do the two expressions reduce to exactly the same value?
               Bound bExpr -> do case exprEq bindings nameCtx expr bExpr of

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

      (termExpr,MatchTerm termName matchArgs)
        -> let (tName,tArgs) = collectTermArgs bindings nameCtx termExpr
              in if tName == termName
                   then patternBindings bindings nameCtx tArgs matchArgs
                   else Nothing -- Matches different term name

      (Sum sumExpr sumIx sumTys, MatchSum ix matchArg)
        | sumIx == ix -> patternBinding bindings nameCtx sumExpr matchArg -- matches this ix. Try match further
        | otherwise   -> Nothing                         -- matches another index

      --
      (Product productExprs, MatchProduct matchArgs)
        -> patternBindings bindings nameCtx productExprs matchArgs -- curry isomorphism?

      (Union unionExpr unionTyIx unionTy, MatchUnion ty matchArg)
        | unionTyIx == ty -> patternBinding bindings nameCtx unionExpr matchArg -- matches this ty. Try match further.
        | otherwise       -> Nothing                           -- matches another type in the union

      -- The entire expression is to be bound
      (expr,Bind)
        -> Just [expr]

  -- The given expression must be either a Term or a Term applied to one or many args.
  -- Return the termName and all args.
  collectTermArgs :: Bindings b abs -> NameCtx -> Expr b abs -> (TermName,[Expr b abs])
  collectTermArgs bindings nameCtx termExpr = second reverse $ collectTermArgs' [] termExpr
    where collectTermArgs' accArgs termArg = let Right reducedTermArg = reduceStep bindings nameCtx termArg in case reducedTermArg of
      -- End of the application to a term
            Term termName
              -> (termName,accArgs)

            -- Applied to at least one more arg...
            App f x
              -> collectTermArgs' (x:accArgs) f

            x -> error $ show (x,bindings)

  -- | Are two expressions identical under the same bindings?
  exprEq :: Bindings b abs -> NameCtx -> Expr b abs -> Expr b abs -> Either Error Bool
  exprEq bindings nameCtx e0 e1 = do
    redE0 <- reduceRec bindings nameCtx e0
    redE1 <- reduceRec bindings nameCtx e1
    Right $ redE0 == redE1

  -- we cant evaluate the case expression yet, so we just reduce all the possible RHSs as much as possible.
  reducePossibleCaseRHSs :: Bindings b abs -> NameCtx -> PossibleCaseBranches b abs -> Either Error (PossibleCaseBranches b abs)
  reducePossibleCaseRHSs bindings nameCtx = \case
    DefaultOnly onMatchExpr
      -> DefaultOnly <$> reduceStep bindings nameCtx onMatchExpr

    CaseBranches branches mDefaultExpr
      -> CaseBranches <$> (reduceSomeBranchRHSs bindings nameCtx branches)
                      <*> maybe (pure Nothing) (Just <$>) (reduceStep bindings nameCtx <$> mDefaultExpr)

  reduceSomeBranchRHSs :: Bindings b abs -> NameCtx -> SomeCaseBranches b abs -> Either Error (SomeCaseBranches b abs)
  reduceSomeBranchRHSs bindings nameCtx (SomeCaseBranches caseBranch caseBranches) =
    SomeCaseBranches <$> (reduceBranchRHS bindings nameCtx caseBranch) <*> (mapM (reduceBranchRHS bindings nameCtx) caseBranches)

  reduceBranchRHS :: Bindings b abs -> NameCtx -> CaseBranch b abs -> Either Error (CaseBranch b abs)
  reduceBranchRHS bindings nameCtx (CaseBranch lhs rhs) = CaseBranch <$> pure lhs <*> reduceStep bindings nameCtx rhs

