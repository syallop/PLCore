{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module PL.Execute where

import PL.Expr
import PL.Error
import PL.Type
import PL.Name
import PL.Var hiding (VarCtx)

import Control.Applicative
import Control.Arrow (second)

import Data.Maybe

-- | A variable context is a list of unbound variables and bound expressions, ordered by distance to their lambda (de brujn index)
--
-- TODO: can probably refactor Varctx & Bound to have a more tree-like structure.
-- Descending under a lambda increments all bound expressions depth by one, we could maybe abstract this and not perform NUMBEROFVARIABLES
-- additions per decend.
type VarCtx = [Bound]

-- | An expression as refered to by a variable is either:
data Bound
  = Bound   Expr Int -- ^ Bound, and has been burried within n lambdas by application. Keeping track here allows 'burying' the free indices at the point of use.
  | UnBound          -- ^ The variable has not been bound to an expression yet => we're looking under an unapplied lambda
  deriving Show

-- | Increment a bound expression's bury depth by one.
-- => we're decending under a lambda.
incBoundDepth :: Bound -> Bound
incBoundDepth = \case
  UnBound   -> UnBound
  Bound e i -> Bound e (i+1)

-- | Within a 'NameCtx', reduce an 'Expr'.
reduce :: NameCtx -> Expr -> Either Error Expr
reduce = reduce' []
  where
  reduce' :: VarCtx -> NameCtx -> Expr -> Either Error Expr
  reduce' varCtx nameCtx expr = case expr of

      -- Terms dont reduce
      Term termName
        -> Right $ Term termName

      -- Vars reduce to whatever they've been bound to, if they've been bound that is.
      Var var
        -> case varCtx !! varToInt var of
              UnBound -> pure $ Var var

              Bound e buryDepth
                {--> reduce' varCtx nameCtx (e `buryBy` buryDepth)-}
                -> pure (e `buryBy` buryDepth)

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
        {--> Lam <$> pure takeTy <*> reduce' (Nothing : varCtx) nameCtx lamExpr-}
        {--> Lam <$> pure takeTy <*> reduce' ((\(v,d) -> (Nothing:v,d+1)) varCtx) nameCtx lamExpr-}
        -> Lam <$> pure takeTy <*> reduce' (UnBound : (map incBoundDepth varCtx)) nameCtx lamExpr

      -- 'strict'
      -- = reduce the argument, then the function, then the result of applying.
      App f x
        -> do x' <- reduce' varCtx nameCtx x
              f' <- reduce' varCtx nameCtx f
              case f' of
                {-Lam _ fExpr -> reduce' (Just x' : varCtx) nameCtx fExpr-}
                Lam _ fExpr -> reduce' ((Bound x' 0) : varCtx) nameCtx fExpr
                Term tName  -> Right $ App (Term tName) x'
                _           -> error "Cant reduce application of non-lambda term"

      -- If the caseExpression is a variable, reduce all of the possible branches
      -- , otherwise, find the first matching branch and bind all matching varibles into the RHS, then reduce it.
      Case caseExpr caseBranches
        -> do caseExpr' <- reduce' varCtx nameCtx caseExpr
              case caseExpr' of
                 Var _ -> Case <$> pure caseExpr' <*> reducePossibleCaseRHSs varCtx nameCtx caseBranches
                 _     -> reducePossibleCaseBranches varCtx nameCtx caseExpr' caseBranches


  -- The expression is not a variable and so we can reduce the case expession, finding the matching branch and returning the reduced RHS.
  reducePossibleCaseBranches :: VarCtx -> NameCtx -> Expr -> PossibleCaseBranches -> Either Error Expr
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
                {--> reduce' ((\(v,d) -> (((map Just $ reverse bindExprs) ++ v),d)) varCtx) nameCtx rhsExpr-}
                -> reduce' ((map (\e -> Bound e 0) $ reverse bindExprs) ++ varCtx) nameCtx rhsExpr

  tryBranches :: VarCtx -> NameCtx -> Expr -> SomeCaseBranches -> Maybe ([Expr],Expr)
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
  tryBranch :: VarCtx -> NameCtx -> Expr -> CaseBranch -> Maybe ([Expr],Expr)
  tryBranch varCtx nameCtx caseExpr caseBranch =
      let (matchArg,rhsExpr) = unCaseBranch caseBranch
         in (,rhsExpr) <$> patternBinding varCtx nameCtx caseExpr matchArg

  -- Try matching all of these expressions against these patterns, return the list of bindings if successful.
  --
  -- (Type checking ensures same length lists and valid patterns)
  patternBindings :: VarCtx -> NameCtx -> [Expr] -> [MatchArg] -> Maybe [Expr]
  patternBindings varCtx nameCtx exprs matchArgs = fmap concat $ mapM (uncurry $ patternBinding varCtx nameCtx) $ zip exprs matchArgs

  -- This expression can be matched by this matchArgs. Return the list of bindings.
  patternBinding :: VarCtx -> NameCtx -> Expr -> MatchArg -> Maybe [Expr]
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
  collectTermArgs :: VarCtx -> NameCtx -> Expr -> (TermName,[Expr])
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
  reducePossibleCaseRHSs :: VarCtx -> NameCtx -> PossibleCaseBranches -> Either Error PossibleCaseBranches
  reducePossibleCaseRHSs varCtx nameCtx = \case
    DefaultOnly onMatchExpr
      -> DefaultOnly <$> reduce' varCtx nameCtx onMatchExpr

    CaseBranches branches mDefaultExpr
      -> CaseBranches <$> (reduceSomeBranchRHSs varCtx nameCtx branches)
                      <*> maybe (pure Nothing) (Just <$>) (reduce' varCtx nameCtx <$> mDefaultExpr)

  reduceSomeBranchRHSs :: VarCtx -> NameCtx -> SomeCaseBranches -> Either Error SomeCaseBranches
  reduceSomeBranchRHSs varCtx nameCtx (SomeCaseBranches caseBranch caseBranches) =
    SomeCaseBranches <$> (reduceBranchRHS varCtx nameCtx caseBranch) <*> (mapM (reduceBranchRHS varCtx nameCtx) caseBranches)

  reduceBranchRHS :: VarCtx -> NameCtx -> CaseBranch -> Either Error CaseBranch
  reduceBranchRHS varCtx nameCtx = \case
    CaseTerm name matches rhsExpr
      -> CaseTerm <$> pure name <*> pure matches <*> (reduce' varCtx nameCtx rhsExpr)

    CaseSum ix match rhsExpr
      -> CaseSum <$> pure ix <*> pure match <*> reduce' varCtx nameCtx rhsExpr

    CaseProd matches rhsExpr
      -> CaseProd <$> pure matches <*> reduce' varCtx nameCtx rhsExpr

    CaseUnion tyIx match rhsExpr
      -> CaseUnion <$> pure tyIx <*> pure match <*> reduce' varCtx nameCtx rhsExpr

-- bury any escaping variables in an expression by given depth.
--
-- E.G.
-- Unaffected as no variables escape.
-- \.0        ~> \.0    --id
-- \.\.1      ~> \.\.1  --const
--
-- Escaping variables are effected.
-- \.1        ~> \.(1+depth)
-- \.\.0 1 2  ~> \.\. 0 1 (2+depth)
--
-- TODO: Refactor code, please..
-- - can probably merge all code into the aux definition/ use mapSubExpr
buryBy :: Expr -> Int -> Expr
buryBy expr 0         = expr
buryBy expr buryDepth = case expr of

  Var v
    -> Var $ intToVar $ (varToInt v) + buryDepth

  Lam ty e
    -> Lam ty (buryBy' 0 e buryDepth)

  App f x
    -> App (buryBy f buryDepth) (buryBy x buryDepth)

  Case caseExpr possibleBranches
    -> Case (buryBy caseExpr buryDepth)
            $ case possibleBranches of
                  DefaultOnly defExpr
                    -> DefaultOnly (buryBy defExpr buryDepth)

                  CaseBranches (SomeCaseBranches caseBranch caseBranches) mExpr
                    -> CaseBranches
                        (SomeCaseBranches (mapCaseRHSs (`buryBy` buryDepth) caseBranch) (map (mapCaseRHSs (`buryBy` buryDepth)) caseBranches))
                        ((`buryBy` buryDepth) <$> mExpr)

  Term termName
    -> Term termName

  Sum sumExpr sumIx sumTys
    -> Sum (buryBy sumExpr buryDepth) sumIx sumTys

  Prod prodExprs
    -> Prod (map (`buryBy` buryDepth) prodExprs)

  Union unionExpr tyIx tys
    -> Union (buryBy unionExpr buryDepth) tyIx tys

  where
    buryBy' :: Int -> Expr -> Int -> Expr
    buryBy' ourTop expr buryDepth = case expr of

      Var v
        -- Variable is within our height
        | (varToInt v) <= ourTop
          -> Var v

        -- Variable escapes our height, so compensate for the greater depth.
        | otherwise
          -> Var $ intToVar $ (varToInt v) + buryDepth

      Lam ty e
        -> Lam ty (buryBy' (ourTop+1) e buryDepth)

      App f x
        -> App (buryBy' ourTop f buryDepth) (buryBy' ourTop x buryDepth)

      Case caseExpr possibleBranches
        -> Case (buryBy' ourTop caseExpr buryDepth)
                $ case possibleBranches of
                      DefaultOnly defExpr
                        -> DefaultOnly (buryBy' ourTop defExpr buryDepth)

                      CaseBranches (SomeCaseBranches caseBranch caseBranches) mExpr
                        -> CaseBranches
                            (SomeCaseBranches (mapCaseRHSs (\e -> buryBy' ourTop e buryDepth) caseBranch) (map (mapCaseRHSs (\e -> buryBy' ourTop e buryDepth)) caseBranches))
                            ((\e -> buryBy' ourTop e buryDepth) <$> mExpr)

      Term termName
        -> Term termName

      Sum sumExpr sumIx sumTys
        -> Sum (buryBy' ourTop sumExpr buryDepth) sumIx sumTys

      Prod prodExprs
        -> Prod (map (\e -> buryBy' ourTop e buryDepth) prodExprs)

      Union unionExpr tyIx tys
        -> Union (buryBy' ourTop unionExpr buryDepth) tyIx tys

