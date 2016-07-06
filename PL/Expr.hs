{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module PL.Expr where

import PL.Case
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Name
import PL.Error
import PL.Kind

import PL.ExprLike
import PL.Binds
import PL.Abstracts
import PL.Bindings

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Control.Applicative

import Debug.Trace

todo :: String -> a
todo str = error $ "TODO: " ++ str

{- Debuging -}
-- trace a string, formatted between braces and indented by 'a few' tabs. As in
-- a mathmatical derivation
traceStep s a = trace ("\t\t\t\t{" ++ s ++ "}") a

-- trace a string, indented a number of tabs
traceIndent i s a = trace (replicate (i*2) ' ' ++ s) a

type BindAbs b abs tb = (Binds b (Type tb), Abstracts abs tb)
type ExprOf b abs tb =  BindAbs b abs tb => Expr b abs tb

-- | Small simply typed lambda calculus with term literals and case analysis.
-- 'abs' is the type of abstractions
-- 'b' is the type of bindings
data Expr b abs tb

    -- | Lambda abstraction
    = Lam
      {_take :: abs
      ,_expr :: Expr b abs tb
      }

    -- | Application
    | App
      {_f :: Expr b abs tb
      ,_x :: Expr b abs tb
      }

    -- | Binding
    | Binding
      {_binding :: b
      }

    -- | Case analysis of an expression
    | CaseAnalysis
      {_caseAnalysis :: Case (Expr b abs tb) (MatchArg b tb)
      }

    -- | An expression is indexed within a Sum type
    --
    -- Currently assuming type guarantees index is within bounds of sumType
    -- (Not unless type-checked that that the expr has that type)
    | Sum
      {_sumExpr  :: Expr b abs tb
      ,_sumIndex :: Int
      ,_sumType  :: [Type tb]
      }

    -- | An expression is a product of many expressions
    | Product
      {_prodExprs :: [Expr b abs tb]
      }

    -- | An expression has one of many unique types
    | Union
      {_unionExpr      :: Expr b abs tb
      ,_unionTypeIndex :: Type tb
      ,_unionType      :: Set.Set (Type tb)
      }

    -- Big lambda abstract type under an expression
    | BigLam
      {_takeTy :: Kind -- replace with tabs
      ,_expr   :: Expr b abs tb
      }

    -- Big application type under an expression
    | BigApp
      {_f   :: Expr b abs tb
      ,_xTy :: Type tb
      }

deriving instance (Eq b,Eq abs,Eq tb) => Eq (Expr b abs tb)

instance (Abstracts abs tb,Show b) => Show (Expr b abs tb) where
  show = showExpr

-- An Expr abstracts over itself
instance HasAbs (Expr b abs tb) where
  applyToAbs f e = case e of
    Lam abs e -> Lam abs (f e)
    e         -> e

-- An Expr has bindings of type 'b'
instance HasBinding (Expr b abs tb) b where
  applyToBinding f e = case e of
    Binding b -> Binding $ f b
    e         -> e

-- An Expr contains NON-abstracted sub-expressions
instance HasNonAbs (Expr b abs tb) where
  applyToNonAbs f e = case e of
    App x y
      -> App (f x) (f y)

    CaseAnalysis c
      -> CaseAnalysis $ mapCaseExpr (applyToNonAbs f) c

    Sum expr ix ty
      -> Sum (f expr) ix ty

    Product prodExprs
      -> Product (map f prodExprs)

    Union unionExpr tyIx ty
      -> Union (f unionExpr) tyIx ty

    BigLam takeTy expr
      -> BigLam takeTy (f expr)

    BigApp fExpr xTy
      -> BigApp (f fExpr) xTy

    e -> e

-- Map a function over all contained subexpressions.
-- The function should preserve the type of the expression.
mapSubExpressions :: (Expr b abs tb -> Expr b abs tb) -> Expr b abs tb -> Expr b abs tb
mapSubExpressions f = \case
  Lam ty expr
    -> Lam ty $ f expr

  App fExpr xExpr
    -> App (f fExpr) (f xExpr)

  CaseAnalysis c
    -> CaseAnalysis $ mapCaseExpr f c

  Sum expr ix ty
    -> Sum (f expr) ix ty

  Product prodExprs
    -> Product (map f prodExprs)

  Union unionExpr tyIx ty
    -> Union (f unionExpr) tyIx ty

  Binding b
    -> Binding b

  BigLam takeTy expr
    -> BigLam takeTy (f expr)

  BigApp fExpr xTy
    -> BigApp (f fExpr) xTy



-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArg b tb
  = MatchSum     Int      (MatchArg b tb)  -- ^ Match against a sum alternative (which may be applied to more patterns)
  | MatchProduct          [MatchArg b tb]  -- ^ Match against a product of many types (which may be applied to more patterns)
  | MatchUnion   (Type tb) (MatchArg b tb) -- ^ Match against a union of alternatives
  | MatchBinding b                         -- ^ Match for exact structural equality
  | Bind                                   -- ^ Match anything and bind it
  deriving (Show,Eq)

showExpr :: forall b abs tb. (Abstracts abs tb, Show b,Show abs,Show tb) => Expr b abs tb -> String
showExpr = \case
  Lam takeTy expr
    -> "(\\" ++ show (absTy takeTy :: Type tb) ++ " " ++ showExpr expr ++ ")"

  App f x
    -> "(@" ++ showExpr f ++ " " ++ showExpr x ++ ")"

  Binding b
    -> show b

  CaseAnalysis c
    -> "(CASE " ++ show c ++ ")"

  Sum sumExpr sumIndex sumType
    -> "(+" ++ show sumIndex ++ " " ++ show sumExpr ++ " " ++ (intercalate " " $ map showType sumType) ++ ")"

  Product prodExprs
    -> "(*" ++ (intercalate " " $ map showExpr prodExprs) ++ ")"

  Union unionExpr unionTypeIndex unionTy
    -> "(U" ++ showType unionTypeIndex ++ " " ++ showExpr unionExpr ++ (intercalate " " $ map showType $ Set.toList unionTy) ++ ")"

  BigLam takeKind expr
    -> "(/\\ " ++ show takeKind ++ " " ++ show expr ++ ")" 

  BigApp fExpr xTy
    -> "(# " ++ show fExpr ++ " " ++ show xTy ++ ")" 


showMatchArgs :: (Show tb,Show b) => [MatchArg b tb] -> String
showMatchArgs = intercalate " " . map showMatchArg

showMatchArg :: (Show b,Show tb) => MatchArg b tb -> String
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
topExprType :: (BindAbs b abs tb,Binds b (Type tb), Binds tb Kind, Ord tb,Show b,Show (BindCtx tb Kind))
            => TypeCtx tb
            -> Expr b abs tb
            -> Either (Error tb) (Type tb)
topExprType = exprType emptyCtx emptyCtx emptyBindings

type ExprBindCtx b tb = BindCtx b (Type tb)
type TypeBindCtx tb   = Binds tb Kind => BindCtx tb Kind
type TypeBindings tb  = Bindings (Type tb)

-- | Under a given bindings context, type check an expression.
exprType :: forall b abs tb. (BindAbs b abs tb,Binds tb Kind,Ord tb,Show b,Show (BindCtx tb Kind))
         => ExprBindCtx b tb -- Associate expr bindings 'b' to their types
         -> TypeBindCtx tb   -- Associate type bindings 'tb' to their Kinds
         -> TypeBindings tb  -- Associate type bindings 'tb' to their bound or unbound types
         -> TypeCtx tb       -- Associate Named types to their TypeInfo
         -> Expr b abs tb    -- Expression to type-check
         -> Either (Error tb) (Type tb)
exprType = exprType' 0

exprType':: forall b abs tb. (BindAbs b abs tb,Binds tb Kind,Ord tb,Show b,Show (BindCtx tb Kind))
         => Int              -- indentation level of debuging output
         -> ExprBindCtx b tb -- Associate expr bindings 'b' to their types
         -> TypeBindCtx tb   -- Associate type bindings 'tb' to their Kinds
         -> TypeBindings tb  -- Associate type bindings 'tb' to their bound or unbound types 
         -> TypeCtx tb       -- Associate Named types to their TypeInfo
         -> Expr b abs tb    -- Expression to type-check
         -> Either (Error tb) (Type tb)
exprType' i exprBindCtx typeBindCtx typeBindings typeCtx e = traceIndent i (show ("EXPRTYPE",e,typeBindCtx,typeBindings)) $ case e of


  -- | ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     expr : exprTy
  -- ----------------------------------
  --   Lam absTy expr : absTy -> exprTy
  Lam abs expr
    -> do let newExprBindCtx = addBinding (absTy abs) exprBindCtx
          exprTy <- exprType' (i+1) newExprBindCtx typeBindCtx typeBindings typeCtx expr
          Right $ Arrow (absTy abs) exprTy

  -- |
  --   f : a -> b    x : a
  -- -----------------------
  --       App f x : b
  App f x
    -> do fTy <- exprType' (i+1) exprBindCtx typeBindCtx typeBindings typeCtx f -- Both f and x must type check
          xTy <- exprType' (i+1) exprBindCtx typeBindCtx typeBindings typeCtx x

          resFTy <- maybe (Left $ EMsg "Unknown named type in function application") Right $ _typeInfoType <$> resolveTypeInitialInfo fTy typeCtx

          {-let errAppMismatch = Left $ EAppMismatch fTy xTy-}
          let errAppMismatch = Left $ error $ show resFTy ++ "   " ++ show xTy
          case resFTy of
            -- Regular function application attempt
            Arrow aTy bTy -> case typeEq typeBindCtx typeBindings typeCtx aTy xTy of
                                 Nothing -> error "Non existant type name in application of arrow type"
                                 Just isSameType
                                   | isSameType -> Right bTy
                                   | otherwise  -> let msg = unlines ["In expr: "
                                                                     ,show $ App f x
                                                                     ,"f : " ++ show fTy
                                                                     ,"x : " ++ show xTy
                                                                     ,"f :~> " ++ show resFTy
                                                                     ]
                                                      in error msg
                                   --error $ "Types not equal in application of arrow type. Expected: " ++ show aTy ++ " Given: " ++ show xTy
                                   --
            _ -> error "Attempting to apply non-arrow type"

  -- Provided an expression type checks and its type is in the correct place within a sum,
  -- has that sum type.
  Sum expr ix inTypr
    -> do -- Expression must type check
          exprTy <- exprType' (i+1) exprBindCtx typeBindCtx typeBindings typeCtx expr

          -- Expression must have the type of the index in the sum it claims to have...
          _ <- case typeEq typeBindCtx typeBindings typeCtx exprTy (inTypr !! ix) of
                   Nothing -> Left $ EMsg "An expressions indexed type in a sum is an unknown type name"
                   Just isSameType
                     | isSameType -> Right ()
                     | otherwise  -> Left $ EMsg $ "Expression doesnt have the type of the position in a sum type it claims it has. Has: " ++ show exprTy ++ " Expected: " ++ show (inTypr !! ix)

          -- Allow the other types in the sum to not exist...
          _ <- Right ()

          -- Type is the claimed sum
          Right $ SumT inTypr

  -- A product is typed by the order of each expression it contains
  Product prodExprs
    -> do -- type check each successive expression
          prodExprTys <- mapM (exprType' (i+1) exprBindCtx typeBindCtx typeBindings typeCtx) prodExprs

          -- the type is the product of those types
          Right $ ProductT prodExprTys

  -- Provided an expression typechecks and its type exists within the union, it has the claimed union type.
  -- TODO: Unused types in the union are not themselves checked for consistency
  -- TODO: The same type appearing more than once should be an error?
  Union unionExpr unionTypeIndex unionTypes
    -> do -- type check injected expression
          exprTy <- exprType' (i+1) exprBindCtx typeBindCtx typeBindings typeCtx unionExpr

          -- Type must be what we claim it is...
          _ <- case typeEq typeBindCtx typeBindings typeCtx exprTy unionTypeIndex of
                   Just True  -> Right ()
                   Just False -> Left $ EMsg "Expression doesnt have the type within the union it claims to have"
                   Nothing    -> Left $ EMsg "A named type in a union doesnt exist"

          -- Type must be in the set somewhere...
          _ <- if Set.member (Just True) . Set.map (\unionTy -> typeEq typeBindCtx typeBindings typeCtx exprTy unionTy) $ unionTypes
                 then Right ()
                 else Left $ EMsg "Expressions type is not within the union"

          -- the type is the claimed union
          Right $ UnionT unionTypes

  -- | A binding is typed by the context
  -- It is assumed the exprBindCtx has been type checked
  --
  -- b : t IN exprBindCtx
  -- -----------------
  --      b : t
  Binding b
    -> case lookupBindingTy b exprBindCtx of
          Nothing -> Left $ EMsg "Expression refers to a non-existant binding"
          Just ty -> Right ty


  --        scrutineeExpr : t0   defExpr : t1
  -- -----------------------------------------------
  --       CASE scrutineeExpr of defExpr  : t1
  --
  --                          or
  --
  --   scrutineeExpr : st   defExpr : dt   branch0 : t   branches : [t]
  -- --------------------------------------------------------------------
  --                     CASE scrutineExpr of
  --                        branch0
  --                        branches            : t
  --                        branches
  --                        ?defExpr
  CaseAnalysis c
    -> do -- scrutinee should be well typed
          scrutineeTy <- exprType' (i+1) exprBindCtx typeBindCtx typeBindings typeCtx $ _caseScrutinee c

          case _caseCaseBranches c of

            -- The case expression is then typed by the default branch, if its well typed
            DefaultOnly defExpr
              -> exprType' (i+1) exprBindCtx typeBindCtx typeBindings typeCtx defExpr

            CaseBranches (branch0 :| branches) mDefExpr
              -> do -- Check the all the branches
                    branch0Ty <- branchType (i+1) branch0 scrutineeTy exprBindCtx typeBindCtx typeBindings typeCtx
                    branchTys <- mapM (\branch -> branchType (i+1) branch scrutineeTy exprBindCtx typeBindCtx typeBindings typeCtx) branches

                    -- Check the default branch if it exists
                    mDefExprTy <- maybe (Right Nothing) (\defExpr -> Just <$> exprType' (i+1) exprBindCtx typeBindCtx typeBindings typeCtx defExpr) mDefExpr

                    -- If the default branch exists, its type must be the same as the first branch
                    _ <- maybe (Right ())
                               (\defExprTy -> case typeEq typeBindCtx typeBindings typeCtx defExprTy branch0Ty of
                                                  Nothing -> Left $ EMsg "First branch has a unresolvable type name"
                                                  Just isSameType
                                                    | isSameType -> Right ()
                                                    | otherwise  -> Left $ EMsg $ unlines ["Default branch and first case branch have different result types"
                                                                                          ,show defExprTy
                                                                                          ,show branch0Ty
                                                                                          ]
                               )
                               mDefExprTy

                    -- Any other branches must have the same type as the first
                    _ <- mapM (\branchTy -> case typeEq typeBindCtx typeBindings typeCtx branchTy branch0Ty of
                                                Nothing -> Left $ EMsg "Branch has an unresolvable type name"
                                                Just isSameType
                                                  | isSameType -> Right ()
                                                  | otherwise  -> Left $ EMsg "Branch and first branch have different result types"
                              )
                              branchTys

                    -- The case expression has the type of all of the branch expressions
                    Right branch0Ty


  --    absKind :: kind      expr : exprTy
  -- ---------------------------------------
  --   BigLam absKind expr : kind BigArrow exprTy
  BigLam abs expr
    -> do let newTypeBindCtx  = addBinding abs typeBindCtx
              newTypeBindings = unbound $ bury typeBindings 
          exprTy <- exprType' (i+1) exprBindCtx newTypeBindCtx newTypeBindings typeCtx expr
          Right $ BigArrow abs exprTy

  --    f : aKind BigLamArrow bTy      xTy :: aKind
  -- ---------------------------------------------------
  --              BigApp f xTy : bTy
  BigApp f x
    -> do xKy <- typeKind typeBindCtx typeCtx x
          let newTypeBindCtx  = addBinding xKy typeBindCtx
              newTypeBindings = bind x typeBindings

          -- Check f under x
          fTy <- exprType' (i+1) exprBindCtx newTypeBindCtx newTypeBindings typeCtx f

          -- TODO maybe verify the xTy we've been given to apply?

          resFTy <- maybe (Left $ EMsg "Unknown named type in Big function application") Right $ _typeInfoType <$> resolveTypeInitialInfo fTy typeCtx

          case resFTy of
            -- Regular big application attempt
            BigArrow aKy bTy
              | aKy == xKy -> Right $ instantiate x bTy -- TODO: all bindings need to be instantiated
              | otherwise  -> Left $ EBigAppMismatch fTy xKy

            _ -> Left $ EMsg "In big application, function must have a big arrow type"

-- Type check a case branch, requiring it match the expected type
-- , if so, type checking the result expression which is returned.
branchType :: (BindAbs b abs tb,Binds tb Kind, Ord tb,Show b,Show (BindCtx tb Kind))
           => Int
           -> CaseBranch (Expr b abs tb) (MatchArg b tb)
           -> Type tb
           -> ExprBindCtx b tb
           -> TypeBindCtx tb
           -> TypeBindings tb
           -> TypeCtx tb
           -> Either (Error tb) (Type tb)
branchType i (CaseBranch lhs rhs) expectedTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  bindings <- checkMatchWith lhs expectedTy exprBindCtx typeBindCtx typeBindings typeCtx
  exprType' (i+1) (addBindings bindings exprBindCtx) typeBindCtx typeBindings typeCtx rhs


-- | Check that a MatchArg matches the expected Type
-- If so, return a list of types of any bound bindings.
checkMatchWith :: (Binds b (Type tb),Binds tb Kind,Ord tb,Show tb)
               => MatchArg b tb
               -> Type tb
               -> ExprBindCtx b tb
               -> TypeBindCtx tb
               -> TypeBindings tb
               -> TypeCtx tb
               -> Either (Error tb) [Type tb]
checkMatchWith match expectTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  rExpectTy <- maybe (Left $ EMsg "The expected type in a pattern is a type name with no definition.") Right $ _typeInfoType <$> resolveTypeInitialInfo expectTy typeCtx
  case match of

    -- Bind the value
    Bind
      -> Right [expectTy]

    MatchBinding b
      -> do -- the type of the binding
            bTy <- maybe (Left $ EMsg "pattern match on a non-existant binding") Right $ lookupBindingTy b exprBindCtx
            case typeEq typeBindCtx typeBindings typeCtx bTy expectTy of
                Nothing         -> Left $ EMsg "pattern match on a Named type which does not exist"
                Just isSameType -> if isSameType then pure [] else Left $ EMsg "pattern match on a binding from a different type"

    MatchSum sumIndex nestedMatchArg
      -> do sumTypes <- case rExpectTy of
                      SumT sumTypes -> Right sumTypes
                      _             -> Left $ EMsg $ "Expected sum type in pattern match"

            -- index must be within the number of alternative in the sum type
            matchedTy <- if length sumTypes < sumIndex then Left $ EMsg "Matching on a larger sum index than the sum type contains" else Right (sumTypes !! sumIndex)

            -- must have the expected index type
            checkMatchWith nestedMatchArg matchedTy exprBindCtx typeBindCtx typeBindings typeCtx

    MatchProduct nestedMatchArgs
      -> do prodTypes <- case rExpectTy of
                             ProductT prodTypes -> Right prodTypes
                             _               -> Left $ EMsg "Expected product type in pattern match"

            checkMatchesWith nestedMatchArgs prodTypes exprBindCtx typeBindCtx typeBindings typeCtx

    MatchUnion unionIndexTy nestedMatchArg
      -> do unionTypes <- case rExpectTy of
                        UnionT unionTypes -> Right unionTypes
                        _                 -> Left $ EMsg "Expected union type in pattern match"

            -- type index must be a member of the union alternatives
            _ <- if Set.member unionIndexTy unionTypes then Right () else Left $ EMsg "Matching on a type which isnt a member of the union"

            -- must actually match on the expected type
            checkMatchWith nestedMatchArg unionIndexTy exprBindCtx typeBindCtx typeBindings typeCtx


checkMatchesWith :: (Binds b (Type tb),Binds tb Kind,Ord tb,Show tb)
                 => [MatchArg b tb]
                 -> [Type tb]
                 -> ExprBindCtx b tb
                 -> TypeBindCtx tb
                 -> TypeBindings tb
                 -> TypeCtx tb
                 -> Either (Error tb) [Type tb]
checkMatchesWith matches types exprBindCtx typeBindCtx typeBindings typeCtx = case (matches,types) of
  ([],[]) -> Right []
  ([],_)  -> Left $ EMsg "Expected more patterns in match"
  (_,[])  -> Left $ EMsg "Too many patterns in match"
  (m:ms,t:ts)
    -> checkMatchWith m t exprBindCtx typeBindCtx typeBindings typeCtx >>= \boundTs -> checkMatchesWith ms ts exprBindCtx typeBindCtx typeBindings typeCtx >>= Right . (boundTs ++)

-- Bind a list of expression with claimed types within an expression
-- by transforming to an application to lambda abstractions.
-- I.E. each subsequent expression
{-lets :: [(Expr,Type)] -> Expr -> Expr-}
{-lets bind exp = foldr (flip App) (foldr Lam exp $ reverse types) exprs-}
  {-where-}
    {-exprs = map fst bind-}
    {-types = map snd bind-}

appise :: [Expr b abs tb] -> Expr b abs tb
appise []        = error "Cant appise empty list of expressions"
appise (e:[])    = e
appise (e:e':es) = appise ((App e e'):es)

lamise :: [abs] -> Expr b abs tb -> Expr b abs tb
lamise []        _ = error "Cant lamise empty list of abstractions"
lamise (t:[])    e = Lam t e
lamise (t:t':ts) e = Lam t (lamise (t':ts) e)

