{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module PL.Expr where

import PL.Type
import PL.TypeCtx
import PL.Name
import PL.Error

import PL.Binds
import PL.Abstracts

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Applicative

todo :: String -> a
todo str = error $ "TODO: " ++ str


type BindAbs b abs = (Binds b, Abstracts abs)
type ExprOf b abs =  BindAbs b abs => Expr b abs

-- | Small simply typed lambda calculus with term literals and case analysis.
-- 'abs' is the type of abstractions
-- 'b' is the type of bindings
data Expr b abs

    -- | Lambda abstraction
    = Lam
      {_take :: abs
      ,_expr :: Expr b abs
      }

    -- | Application
    | App
      {_f :: Expr b abs
      ,_x :: Expr b abs
      }

    -- | Binding
    | Binding
      {_binding :: b
      }

    -- | Case analysis of an expression
    | Case
      {_caseExpr     :: Expr b abs
      ,_caseBranches :: PossibleCaseBranches b abs
      }

    -- | An expression is indexed within a Sum type
    --
    -- Currently assuming type guarantees index is within bounds of sumType
    -- (Not unless type-checked that that the expr has that type)
    | Sum
      {_sumExpr  :: Expr b abs
      ,_sumIndex :: Int
      ,_sumType  :: [Type]
      }

    -- | An expression is a product of many expressions
    | Product
      {_prodExprs :: [Expr b abs]
      }

    -- | An expression has one of many unique types
    | Union
      {_unionExpr      :: Expr b abs
      ,_unionTypeIndex :: Type
      ,_unionType      :: Set.Set Type
      }

deriving instance (Binds b, Abstracts abs) => Eq (Expr b abs)

instance (Binds b,Abstracts abs) => Show (Expr b abs) where
  show = showExpr

-- | Body of a case expression. Is either:
-- - Just a catch all match
-- - One or many branches and a possible default catch all
data PossibleCaseBranches b abs

  -- | No proper matches, only a default catch all
  -- case ... of
  --   {_ -> exp}
  = DefaultOnly
    { _onMatch :: Expr b abs
    }

  -- | One or many branches and a possible default catch all
  -- case ... of
  --   {pat -> exp
  --    ... -> ...
  --    ... -> ...
  --    _   -> ...
  --   }
  | CaseBranches
    { _branches       :: SomeCaseBranches b abs -- One to many
    , _defaultOnMatch :: Maybe (Expr b abs) -- Possible catch all default
    }
    deriving (Show, Eq)

-- | One or many 'CaseBranch'
data SomeCaseBranches b abs = SomeCaseBranches (CaseBranch b abs) [CaseBranch b abs]
  deriving (Show,Eq)

-- | A single branch in a case analysis of a type
data CaseBranch b abs = CaseBranch
  {_caseLHS :: MatchArg b
  ,_caseRHS :: Expr b abs
  }
  deriving (Show,Eq)


-- Map a monadic function over all the sub expressions within an expression
{-exprMapM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr-}

-- Map a function over all contained subexpressions.
-- The function should preserve the type of the expression.
mapSubExpressions :: (Expr b abs -> Expr b abs) -> Expr b abs -> Expr b abs
mapSubExpressions f = \case
  Lam ty expr
    -> Lam ty $ f expr

  App fExpr xExpr
    -> App (f fExpr) (f xExpr)

  Case caseExpr possibleBranches
    -> Case (f caseExpr) (case possibleBranches of
                             DefaultOnly branchExpr
                               -> DefaultOnly (f branchExpr)

                             CaseBranches (SomeCaseBranches branch branches) mExpr
                               -> CaseBranches (SomeCaseBranches (mapCaseRHSs f branch) (map (mapCaseRHSs f) branches)) (f <$> mExpr)
                         )

  Sum expr ix ty
    -> Sum (f expr) ix ty

  Product prodExprs
    -> Product (map f prodExprs)

  Union unionExpr tyIx ty
    -> Union (f unionExpr) tyIx ty

  Binding b
    -> Binding b

mapCaseRHSs :: (Expr b abs -> Expr b abs) -> CaseBranch b abs -> CaseBranch b abs
mapCaseRHSs f (CaseBranch lhs rhs) = CaseBranch lhs (f rhs)



-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArg b
  = MatchSum     Int      (MatchArg b) -- ^ Match against a sum alternative (which may be applied to more patterns)
  | MatchProduct          [MatchArg b] -- ^ Match against a product of many types (which may be applied to more patterns)
  | MatchUnion   Type     (MatchArg b) -- ^ Match against a union of alternatives
  | MatchBinding b                     -- ^ Match for exact structural equality
  | Bind                               -- ^ Match anything and bind it
  deriving (Show,Eq)

showExpr :: (BindAbs b abs,Show b,Show abs) => Expr b abs -> String
showExpr = \case
  Lam takeTy expr
    -> "(\\" ++ show (absTy takeTy) ++ " " ++ showExpr expr ++ ")"

  App f x
    -> "(@" ++ showExpr f ++ " " ++ showExpr x ++ ")"

  Binding b
    -> show b

  Case caseExpr caseBranches
    -> "(CASE" ++ showExpr caseExpr ++ "\n" ++ showPossibleCaseBranches caseBranches ++ ")"

  Sum sumExpr sumIndex sumType
    -> "(+" ++ show sumIndex ++ " " ++ show sumExpr ++ " " ++ (intercalate " " $ map showType sumType) ++ ")"

  Product prodExprs
    -> "(*" ++ (intercalate " " $ map showExpr prodExprs) ++ ")"

  Union unionExpr unionTypeIndex unionTy
    -> "(U" ++ showType unionTypeIndex ++ " " ++ showExpr unionExpr ++ (intercalate " " $ map showType $ Set.toList unionTy) ++ ")"

showPossibleCaseBranches :: BindAbs b abs => PossibleCaseBranches b abs -> String
showPossibleCaseBranches = \case
  DefaultOnly onMatch
    -> showExpr onMatch

  CaseBranches someCaseBranches maybeDefaultOnMatch
    -> showSomeCaseBranches someCaseBranches ++ "\n" ++ (maybe "" showExpr maybeDefaultOnMatch)

showSomeCaseBranches :: BindAbs b abs => SomeCaseBranches b abs -> String
showSomeCaseBranches (SomeCaseBranches caseBranch caseBranches) = intercalate "\n" $ map showCaseBranch (caseBranch : caseBranches)

showCaseBranch :: BindAbs b abs => CaseBranch b abs -> String
showCaseBranch (CaseBranch lhs rhs) = showMatchArg lhs ++ " " ++ showExpr rhs

showMatchArgs :: Binds b => [MatchArg b] -> String
showMatchArgs = intercalate " " . map showMatchArg

showMatchArg :: Binds b => MatchArg b -> String
showMatchArg = \case
  MatchSum ix matchArg
    -> "+" ++ show ix ++ " " ++ showMatchArg matchArg

  MatchProduct matchArgs
    -> "*" ++ (intercalate " " $ map showMatchArg matchArgs)

  MatchUnion ty matchArg
    -> "U" ++ showType ty ++ " " ++ showMatchArg matchArg

  MatchBinding b
    -> show b

  Bind
    -> "?"

-- | A top-level expression is an expression without a bindings context.
topExprType :: BindAbs b abs => TypeCtx -> Expr b abs -> Either Error Type
topExprType = exprType emptyCtx

-- | Under a given bindings context, type check an expression.
exprType :: forall b abs. BindAbs b abs => BindCtx b -> TypeCtx -> Expr b abs -> Either Error Type
exprType bindCtx typeCtx e = case e of


  -- | ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     expr : exprTy
  -- ----------------------------------
  --   Lam absTy expr : absTy -> exprTy
  Lam abs expr
    -> do let newBindCtx = addBinding (absTy abs) bindCtx
          exprTy <- exprType newBindCtx typeCtx expr
          Right $ Arrow (absTy abs) exprTy

  -- |
  --   f : a -> b    x : a
  -- -----------------------
  --       App f x : b
  App f x
    -> do fTy <- exprType bindCtx typeCtx f -- Both f and x must type check
          xTy <- exprType bindCtx typeCtx x

          resFTy <- maybe (Left $ EMsg "Unknown named type in function application") Right $ resolveInitialType fTy typeCtx

          let errAppMismatch = Left $ EAppMismatch fTy xTy
          case resFTy of
            -- Regular function application attempt
            Arrow aTy bTy -> case typeEq aTy xTy typeCtx of
                                 Nothing -> errAppMismatch
                                 Just isSameType
                                   | isSameType -> Right bTy
                                   | otherwise  -> errAppMismatch
            _ -> errAppMismatch

  -- Provided an expression type checks and its type is in the correct place within a sum,
  -- has that sum type.
  Sum expr ix inTypr
    -> do -- Expression must type check
          exprTy <- exprType bindCtx typeCtx expr

          -- Expression must have the type of the index in the sum it claims to have...
          _ <- case typeEq exprTy (inTypr !! ix) typeCtx of
                   Nothing -> Left $ EMsg "An expressions indexed type in a sum is an unknown type name"
                   Just isSameType
                     | isSameType -> Right ()
                     | otherwise  -> Left $ EMsg "Expression doesnt have the type of the position in a sum type it claims it has"

          -- Allow the other types in the sum to not exist...
          _ <- Right ()

          -- Type is the claimed sum
          Right $ SumT inTypr

  -- A product is typed by the order of each expression it contains
  Product prodExprs
    -> do -- type check each successive expression
          prodExprTys <- mapM (exprType bindCtx typeCtx) prodExprs

          -- the type is the product of those types
          Right $ ProductT prodExprTys

  -- Provided an expression typechecks and its type exists within the union, it has the claimed union type.
  -- TODO: Unused types in the union are not themselves checked for consistency
  -- TODO: The same type appearing more than once should be an error?
  Union unionExpr unionTypeIndex unionTypes
    -> do -- type check injected expression
          exprTy <- exprType bindCtx typeCtx unionExpr

          -- Type must be what we claim it is...
          _ <- case typeEq exprTy unionTypeIndex typeCtx of
                   Just True  -> Right ()
                   Just False -> Left $ EMsg "Expression doesnt have the type within the union it claims to have"
                   Nothing    -> Left $ EMsg "A named type in a union doesnt exist"

          -- Type must be in the set somewhere...
          _ <- if Set.member (Just True) . Set.map (\unionTy -> typeEq exprTy unionTy typeCtx) $ unionTypes
                 then Right ()
                 else Left $ EMsg "Expressions type is not within the union"

          -- the type is the claimed union
          Right $ UnionT unionTypes

  -- | A binding is typed by the context
  -- It is assumed the bindCtx has been type checked
  --
  -- b : t IN bindCtx
  -- -----------------
  --      b : t
  Binding b
    -> case b `bindTy` bindCtx of
          Nothing -> Left $ EMsg "Expression refers to a non-existant binding"
          Just ty -> Right ty

  -- | A case expression with only a defaut branch.
  --
  -- caseExpr : ct   defExpr : t
  -- --------------------------
  --   case caseExpr Of
  --      defExpr        : t
  Case caseExpr (DefaultOnly defExpr)
    -> do -- caseExpr should be well typed (but we don't care about anything else)
          _ <- exprType bindCtx typeCtx caseExpr

          -- The case expression is then typed by the default branch assuming its well typed
          exprType bindCtx typeCtx defExpr


  -- | A case expression with one or many branches and a possible default branch.
  --
  -- caseExpr : ct    defExpr : dt    branch0 : t     branches : [t]
  -- -------------------------------------------------------------
  --                   case caseExpr of
  --                     branch0
  --                     branches      : t
  --                     mDefExpr
  Case caseExpr (CaseBranches (SomeCaseBranches branch0 branches) mDefExpr)
    -> do -- The expression case-analysed on must type-check
          caseExprTy <- exprType bindCtx typeCtx caseExpr

          -- Check the first and any other branches
          branch0Ty <- branchType branch0 caseExprTy bindCtx typeCtx
          branchTys <- mapM (\branch -> branchType branch caseExprTy bindCtx typeCtx) branches

          -- Check the default branch if it exists
          mDefExprTy <- maybe (Right Nothing) (\defExpr -> Just <$> exprType bindCtx typeCtx defExpr) mDefExpr

          -- Check all branches have the same result type
          -- If the default branch exists, its type must be the same as the first branch
          _ <- maybe (Right ())
                     (\defExprTy -> case typeEq defExprTy branch0Ty typeCtx of
                                        Nothing -> Left $ EMsg "First branch has a unresolvable type name"
                                        Just isSameType
                                          | isSameType -> Right ()
                                          | otherwise  -> Left $ EMsg "Default branch and first case branch have different result types"
                     )
                     mDefExprTy

          -- Any other branches must have the same type as the first
          _ <- mapM (\branchTy -> case typeEq branchTy branch0Ty typeCtx of
                                      Nothing -> Left $ EMsg "Branch has an unresolvable type name"
                                      Just isSameType
                                        | isSameType -> Right ()
                                        | otherwise  -> Left $ EMsg "Branch and first branch have different result types"
                    )
                    branchTys

          Right branch0Ty
          -- TODO: maybe check coverage...

-- Type check a case branch, requring it match the expected type
-- if so, type checking the result expression which is returned
branchType :: BindAbs b abs => CaseBranch b abs -> Type -> BindCtx b -> TypeCtx -> Either Error Type
branchType (CaseBranch lhs rhs) expectedTy bindCtx typeCtx = do
  bindings <- checkMatchWith lhs expectedTy bindCtx typeCtx
  exprType (addBindings bindings bindCtx) typeCtx rhs

-- | Check that a MatchArg matches the expected Type
-- If so, return a list of types of any bound bindings.
checkMatchWith :: Binds b => MatchArg b -> Type -> BindCtx b -> TypeCtx -> Either Error [Type]
checkMatchWith match expectTy bindCtx typeCtx = do
  rExpectTy <- maybe (Left $ EMsg "The expected type in a pattern is a type name with no definition.") Right $ resolveInitialType expectTy typeCtx
  case match of

    -- Bind the value
    Bind
      -> Right [expectTy]

    MatchBinding b
      -> do -- the type of the binding
            bTy <- maybe (Left $ EMsg "pattern match on a non-existant binding") Right $ bindTy b bindCtx
            case typeEq bTy expectTy typeCtx of
                Nothing         -> Left $ EMsg "pattern match on a Named type which does not exist"
                Just isSameType -> if isSameType then pure [] else Left $ EMsg "pattern match on a binding from a different type"

    MatchSum sumIndex nestedMatchArg
      -> do sumTypes <- case rExpectTy of
                      SumT sumTypes -> Right sumTypes
                      _             -> Left $ EMsg $ "Expected sum type in pattern match"

            -- index must be within the number of alternative in the sum type
            matchedTy <- if length sumTypes < sumIndex then Left $ EMsg "Matching on a larger sum index than the sum type contains" else Right (sumTypes !! sumIndex)

            -- must have the expected index type
            checkMatchWith nestedMatchArg matchedTy bindCtx typeCtx

    MatchProduct nestedMatchArgs
      -> do prodTypes <- case rExpectTy of
                             ProductT prodTypes -> Right prodTypes
                             _               -> Left $ EMsg "Expected product type in pattern match"

            checkMatchesWith nestedMatchArgs prodTypes bindCtx typeCtx

    MatchUnion unionIndexTy nestedMatchArg
      -> do unionTypes <- case rExpectTy of
                        UnionT unionTypes -> Right unionTypes
                        _                 -> Left $ EMsg "Expected union type in pattern match"

            -- type index must be a member of the union alternatives
            _ <- if Set.member unionIndexTy unionTypes then Right () else Left $ EMsg "Matching on a type which isnt a member of the union"

            -- must actually match on the expected type
            checkMatchWith nestedMatchArg unionIndexTy bindCtx typeCtx


checkMatchesWith :: Binds b => [MatchArg b] -> [Type] -> BindCtx b -> TypeCtx -> Either Error [Type]
checkMatchesWith matches types bindCtx typeCtx = case (matches,types) of
  ([],[]) -> Right []
  ([],_)  -> Left $ EMsg "Expected more patterns in match"
  (_,[])  -> Left $ EMsg "Too many patterns in match"
  (m:ms,t:ts)
    -> checkMatchWith m t bindCtx typeCtx >>= \boundTs -> checkMatchesWith ms ts bindCtx typeCtx >>= Right . (boundTs ++)

-- Bind a list of expression with claimed types within an expression
-- by transforming to an application to lambda abstractions.
-- I.E. each subsequent expression
{-lets :: [(Expr,Type)] -> Expr -> Expr-}
{-lets bind exp = foldr (flip App) (foldr Lam exp $ reverse types) exprs-}
  {-where-}
    {-exprs = map fst bind-}
    {-types = map snd bind-}

appise :: [Expr b abs] -> Expr b abs
appise []        = error "Cant appise empty list of expressions"
appise (e:[])    = e
appise (e:e':es) = appise ((App e e'):es)

lamise :: BindAbs b abs => [abs] -> Expr b abs -> Expr b abs
lamise []        _ = error "Cant lamise empty list of abstractions"
lamise (t:[])    e = Lam t e
lamise (t:t':ts) e = Lam t (lamise (t':ts) e)

