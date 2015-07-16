{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module PL.Execute where

import PL.Expr
import PL.Error
import PL.Type
import PL.Name
import PL.Var

import Control.Applicative
import Control.Arrow (second)

import Data.Maybe

-- | Within a 'NameCtx', reduce an 'Expr'.
reduce :: NameCtx -> Expr -> Either Error Expr
reduce = reduce' []
  where
  reduce' :: [Maybe Expr] -> NameCtx -> Expr -> Either Error Expr
  reduce' varCtx nameCtx expr = case expr of

      -- Terms dont reduce
      Term termName
        -> Right $ Term termName

      -- Vars reduce to whatever they've been bound to, if they've been bound that is.
      Var var
        -> pure $ case varCtx !! varToInt var of
              Nothing -> Var var
              Just e  -> e

      -- Reduce the sums expression
      Sum sumExpr sumIx sumTy
        -> Sum <$> reduce' varCtx nameCtx sumExpr <*> pure sumIx <*> pure sumTy

      -- Reduce all prod expressions
      Prod prodExprs
        -> Prod <$> mapM (reduce' varCtx nameCtx) prodExprs

      -- Reduce the unionExpr
      Union unionExpr unionTyIx unionTy
        -> Union <$> reduce' varCtx nameCtx unionExpr <*> pure unionTyIx <*> pure unionTy

      -- Reduce under the lambda
      Lam takeTy lamExpr
        -> Lam <$> pure takeTy <*> reduce' (Nothing : varCtx) nameCtx lamExpr

      -- 'strict'
      -- = reduce the argument, then the function, then the result of applying.
      App f x
        -> do x' <- reduce' varCtx nameCtx x
              f' <- reduce' varCtx nameCtx f
              case f' of
                Lam _ fExpr -> reduce' (Just x' : varCtx) nameCtx fExpr
                _           -> error "Cant reduce application of non-lambda term"

      -- If the caseExpression is a variable, reduce all of the possible branches
      -- , otherwise, find the first matching branch and bind all matching varibles into the RHS, then reduce it.
      Case caseExpr caseBranches
        -> do caseExpr' <- reduce' varCtx nameCtx caseExpr
              case caseExpr' of
                 Var _ -> Case <$> pure caseExpr' <*> reducePossibleCaseRHSs varCtx nameCtx caseBranches
                 _     -> reducePossibleCaseBranches varCtx nameCtx caseExpr caseBranches


  -- The expression is not a variable and so we can reduce the case expession, finding the matching branch and returning the reduced RHS.
  reducePossibleCaseBranches :: [Maybe Expr] -> NameCtx -> Expr -> PossibleCaseBranches -> Either Error Expr
  reducePossibleCaseBranches varCtx nameCtx caseExpr = \case
      DefaultOnly defaultExpr
        -> reduce' varCtx nameCtx defaultExpr

      CaseBranches branches mDefaultExpr
        -> case (tryBranches varCtx nameCtx caseExpr branches,mDefaultExpr) of

              -- No branch matches, so default should exist and be used.
              (Nothing,Just defaultExpr)
                 -> reduce' varCtx nameCtx defaultExpr

              -- Branch matches, reduce its rhs under any bindings.
              (Just (bindExprs,rhsExpr),_)
                -> reduce' ((map Just $ reverse bindExprs) ++ varCtx) nameCtx rhsExpr

  tryBranches :: [Maybe Expr] -> NameCtx -> Expr -> SomeCaseBranches -> Maybe ([Expr],Expr)
  tryBranches varCtx nameCtx caseExpr (SomeCaseBranches caseBranch caseBranches) =
      firstMatch $ tryBranch varCtx nameCtx caseExpr <$> (caseBranch : caseBranches)

  firstMatch :: [Maybe a] -> Maybe a
  firstMatch = safeHead . map fromJust . filter isJust

  safeHead (x:xs) = Just x
  safeHead []     = Nothing

  -- Try and match a branch against an expression.
  -- On success return a list of all expressions to be bound under a RHS expression.
  --
  -- We assume the input is type-checked which ensures patterns have the same type as the caseexpr matched
  -- upon, and all patterns are completly valid.
  tryBranch :: [Maybe Expr] -> NameCtx -> Expr -> CaseBranch -> Maybe ([Expr],Expr)
  tryBranch varCtx nameCtx caseExpr caseBranch =
      let (matchArg,rhsExpr) = unCaseBranch caseBranch
         in (,rhsExpr) <$> patternBinding varCtx nameCtx caseExpr matchArg

  -- Try matching all of these expressions against these patterns, return the list of bindings if successful.
  --
  -- (Type checking ensures same length lists and valid patterns)
  patternBindings :: [Maybe Expr] -> NameCtx -> [Expr] -> [MatchArg] -> Maybe [Expr]
  patternBindings varCtx nameCtx exprs matchArgs = fmap concat $ mapM (uncurry $ patternBinding varCtx nameCtx) $ zip exprs matchArgs

  -- This expression can be matched by this matchArgs. Return the list of bindings.
  patternBinding :: [Maybe Expr] -> NameCtx -> Expr -> MatchArg -> Maybe [Expr]
  patternBinding varCtx nameCtx expr matchArg = case (expr,matchArg) of

      (termExpr,MatchTerm termName matchArgs)
        -> let (tName,tArgs) = collectTermArgs varCtx nameCtx termExpr
              in if tName == termName
                   then patternBindings varCtx nameCtx tArgs matchArgs
                   else Nothing -- Matches different term name

      (Sum sumExpr sumIx sumTys, MatchSum ix matchArg)
        | sumIx == ix -> patternBinding varCtx nameCtx sumExpr matchArg -- matches this ix. Try match further
        | otherwise   -> Nothing                         -- matches another index

      --
      (Prod prodExprs, MatchProd matchArgs)
        -> patternBindings varCtx nameCtx prodExprs matchArgs -- curry isomorphism?

      (Union unionExpr unionTyIx unionTy, MatchUnion ty matchArg)
        | unionTyIx == ty -> patternBinding varCtx nameCtx unionExpr matchArg -- matches this ty. Try match further.
        | otherwise       -> Nothing                           -- matches another type in the union

      -- The entire expression is to be bound
      (expr,BindVar)
        -> Just [expr]

  -- The given expression must be either a Term or a Term applied to one or many args.
  -- Return the termName and all args.
  collectTermArgs :: [Maybe Expr] -> NameCtx -> Expr -> (TermName,[Expr])
  collectTermArgs varCtx nameCtx termExpr = second reverse $ collectTermArgs' [] termExpr
    where collectTermArgs' accArgs termArg = let Right reducedTermArg = reduce' varCtx nameCtx termArg in case reducedTermArg of
      -- End of the application to a term
            Term termName
              -> (termName,accArgs)

            -- Applied to at least one more arg...
            App f x
              -> collectTermArgs' (x:accArgs) f

            x -> error $ show (x,varCtx)




  -- we cant evaluate the case expression yet, so we just reduce all the possible RHSs as much as possible.
  reducePossibleCaseRHSs :: [Maybe Expr] -> NameCtx -> PossibleCaseBranches -> Either Error PossibleCaseBranches
  reducePossibleCaseRHSs varCtx nameCtx = \case
    DefaultOnly onMatchExpr
      -> DefaultOnly <$> reduce' varCtx nameCtx onMatchExpr

    CaseBranches branches mDefaultExpr
      -> CaseBranches <$> (reduceSomeBranchRHSs varCtx nameCtx branches)
                      <*> maybe (pure Nothing) (Just <$>) (reduce' varCtx nameCtx <$> mDefaultExpr)

  reduceSomeBranchRHSs :: [Maybe Expr] -> NameCtx -> SomeCaseBranches -> Either Error SomeCaseBranches
  reduceSomeBranchRHSs varCtx nameCtx (SomeCaseBranches caseBranch caseBranches) =
    SomeCaseBranches <$> (reduceBranchRHS varCtx nameCtx caseBranch) <*> (mapM (reduceBranchRHS varCtx nameCtx) caseBranches)

  reduceBranchRHS :: [Maybe Expr] -> NameCtx -> CaseBranch -> Either Error CaseBranch
  reduceBranchRHS varCtx nameCtx = \case
    CaseTerm name matches rhsExpr
      -> CaseTerm <$> pure name <*> pure matches <*> (reduce' varCtx nameCtx rhsExpr)

    CaseSum ix match rhsExpr
      -> CaseSum <$> pure ix <*> pure match <*> reduce' varCtx nameCtx rhsExpr

    CaseProd matches rhsExpr
      -> CaseProd <$> pure matches <*> reduce' varCtx nameCtx rhsExpr

    CaseUnion tyIx match rhsExpr
      -> CaseUnion <$> pure tyIx <*> pure match <*> reduce' varCtx nameCtx rhsExpr

