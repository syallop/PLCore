{-# LANGUAGE
     ConstraintKinds
   , FlexibleContexts
   , FlexibleInstances
   , GADTs
   , LambdaCase
   , MultiParamTypeClasses
   , OverloadedStrings
   , RankNTypes
   , ScopedTypeVariables
   , StandaloneDeriving
   , DeriveAnyClass
   #-}
{-|
Module      : PL.Expr
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

An AST containing anonymous functions, sums, products and union types.
Indexed by de bruijn indexes and with some level of type functions.
-}
module PL.Expr where

import PL.Abstracts
import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.ExprLike
import PL.FixExpr
import PL.Kind
import PL.Name
import PLPrinter
import PLPrinter.Doc
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.FixType
import PL.TypeCtx

import Control.Applicative
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid hiding (Sum,Product)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | BindAbs is a synonym for the constraints that:
-- - 'b' Binds 'Type tb'
-- - 'abs' Abstracts 'tb'
type BindAbs b abs tb = (Binds b (Type tb), Abstracts abs tb)

-- | A typed lambda calculus with anonymous sums, products, unions,
-- type level functions and case analysis.
--
-- - 'b' is the type of bindings which refer to an 'abs'traction.
--   In many languages, this might be a variable name "foo".
--   Current uses of this AST use De Bruijn indexes, I.E. a natural number which
--   points a number of binders away to an 'abs'traction "0","1", etc.
--
-- - 'abs' is the type of things a lambda abstracts.
--   In many languages, this might be a new variable name, perhaps with a type
--   annotation.
--   Current uses of this AST use a type signature and use De Bruijn indexes in
--   binders to refer to a variable of the given type.
--
-- - 'tb' is the type of type-level bindings. 'b' - but at the type level.
type Expr b abs tb = FixExpr b abs tb ExprF

-- | A typed lambda calculus with anonymous sums, products, unions,
-- type level functions and case analysis.
--
-- - 'b' is the type of bindings which refer to an 'abs'traction.
--   In many languages, this might be a variable name "foo".
--   Current uses of this AST use De Bruijn indexes, I.E. a natural number which
--   points a number of binders away to an 'abs'traction "0","1", etc.
--
-- - 'abs' is the type of things a lambda abstracts.
--   In many languages, this might be a new variable name, perhaps with a type
--   annotation.
--   Current uses of this AST use a type signature and use De Bruijn indexes in
--   binders to refer to a variable of the given type.
--
-- - 'tb' is the type of type-level bindings. 'b' - but at the type level.
--
-- - 'expr' is the recursive type of subexpressions. This is usually
--   instantiated using FixExpr such that it refers to itself.
--   This allows us to use recursion schemes.
--
--   The examples used in constructor comments use completly arbitrary syntax.
data ExprF b abs tb expr

    -- | Lambda abstraction
    --
    -- Accepts some 'abs'tracted expr which is then bound under the resulting
    -- 'expr' and can be referenced by 'b'.
    --
    -- E.G.
    -- \(x::Int). x+x
    = Lam
      { _take :: abs
      , _expr :: expr
      }

    -- | Application
    --
    -- Apply an expression to another.
    --
    -- E.G.
    -- (\x -> x) 1
    | App
      { _f :: expr
      , _x :: expr
      }

    -- | Binding
    --
    -- Bind the expression refered to by 'b'.
    --
    -- E.G.
    --
    -- varname
    --
    -- where 'varname' should have been bound by some lambda.
    | Binding
      { _binding :: b
      }

    -- | Case analysis of an expression.
    --
    -- An 'expr'ession can be scrutinised, and matches attempted against a
    -- series of patterns. When a pattern matches, any expressions matched in
    -- the pattern are bound in its expression.
    --
    -- E.G.
    --
    -- case (1,2) of
    --   (2,1) -> "not matched"
    --   (1,_) -> "matched"
    --   (1,2) -> "Match doesnt continue this far"
    --   _     -> "default case"
    | CaseAnalysis
      { _caseAnalysis :: (Case expr (MatchArg b tb))
      }

    -- | A Sum is an expression indexed within an ordered collection of
    -- expressions types.
    --
    -- Type checkers may currently assume the index is within the bounds of the
    -- sum when type checking the expr.
    --
    -- E.G.
    --
    -- true :: 1 :: Bool|Int|Char
    --
    -- Is an expression at the first index within Bool,Int and Char (I.E. Bool).
    | Sum
      { _sumExpr  :: expr
      , _sumIndex :: Int
      , _sumType  :: NonEmpty (Type tb)
      }

    -- | An Product is many ordered expressions.
    --
    -- E.G.
    --
    -- (true, 1, 'a') :: (Bool, Int, Char)
    --
    -- Is a product of Bool, Int and Char.
    | Product
      { _prodExprs :: [expr]
      }

    -- | A Union is an expression indexed within an unordered, unique collection
    -- of expression types by its type.
    --
    -- E.G.
    -- x :: Bool :: {Char, Bool, Int}
    --
    -- Is a variable with type Bool within a Union of Char,Bool and Int.
    | Union
      { _unionExpr      :: expr
      , _unionTypeIndex :: Type tb
      , _unionType      :: Set.Set (Type tb)
      }

    -- | Big lambda - bind a type in an expression.
    --
    -- Accepts an abstraction type with a 'Kind' which is then bound under the
    -- resulting 'expr'ession and can be referenced by 'tb' in a 'Type'.
    -- abstract type under an expression
    --
    -- E.G.
    --
    -- \(t :: Kind). (\(x :: t) x)
    --
    -- Should take a type with Kind and produce an expression which takes an
    -- expression of that type, returning it.
    | BigLam
      { _takeTy :: Kind -- TODO: Replace with 'tabs' mirroring 'abs'.
      , _expr   :: expr
      }

    -- | Big application - apply a type to an expression.
    --
    -- Apply a 'Type tb' to an 'expr'ession.
    --
    -- E.G.
    --
    -- (\(t :: Kind). (\(x :: t) x)) Bool
    --
    -- Should evaluate to a function which accepts a bool expression and returns
    -- it.
    | BigApp
      { _f   :: expr
      , _xTy :: Type tb
      }
      deriving Show

deriving instance (Eq b,Eq abs,Eq tb,Eq expr) => Eq (ExprF b abs tb expr)

-- TODO: Recursion schemes can probably be used for these instances now.

-- An Expr abstracts over itself
instance HasAbs (Expr b abs tb) where
  applyToAbs f e = FixExpr $ case unfixExpr e of
    Lam abs e -> Lam abs (f e)
    e         -> e

-- An Expr has bindings of type 'b'
instance HasBinding (Expr b abs tb) b where
  applyToBinding f e = FixExpr $ case unfixExpr e of
    Binding b -> Binding $ f b
    e         -> e

-- An Expr contains NON-abstracted sub-expressions
instance HasNonAbs (Expr b abs tb) where
  applyToNonAbs f e = FixExpr $ case unfixExpr e of
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
mapSubExpressions
  :: (Expr b abs tb -> Expr b abs tb)
  -> Expr b abs tb
  -> Expr b abs tb
mapSubExpressions f e = fixExpr $ case unfixExpr e of
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
  = MatchSum     Int        (MatchArg b tb) -- ^ Match against a sum alternative (which may be applied to more patterns)
  | MatchProduct            [MatchArg b tb] -- ^ Match against a product of many types (which may be applied to more patterns)
  | MatchUnion   (Type tb) (MatchArg b tb)  -- ^ Match against a union of alternatives
  | MatchBinding b                          -- ^ Match for exact structural equality
  | Bind                                    -- ^ Match anything and bind it
  deriving (Show,Eq)

-- | A top-level expression is an expression without a bindings context.
topExprType
  :: ( BindAbs b abs tb
     , Binds b (Type tb)
     , Binds tb Kind
     , Ord tb
     )
  => TypeCtx tb
  -> Expr b abs tb
  -> Either (Error tb) (Type tb)
topExprType = exprType emptyCtx emptyCtx emptyBindings

type ExprBindCtx b tb = BindCtx b (Type tb)
type TypeBindCtx tb   = Binds tb Kind => BindCtx tb Kind
type TypeBindings tb  = Bindings (Type tb)

-- | Under a binding context, type check an expression.
exprType
  :: forall b abs tb
   . ( BindAbs b abs tb
     , Binds tb Kind
     , Ord tb
     )
  => ExprBindCtx b tb -- Associate expr bindings 'b' to their types
  -> TypeBindCtx tb   -- Associate type bindings 'tb' to their Kinds
  -> TypeBindings tb  -- Associate type bindings 'tb' to their bound or unbound types
  -> TypeCtx tb       -- Associate Named types to their TypeInfo
  -> Expr b abs tb    -- Expression to type-check
  -> Either (Error tb) (Type tb)
exprType exprBindCtx typeBindCtx typeBindings typeCtx e = case unfixExpr e of

  -- | ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     expr : exprTy
  -- ----------------------------------
  --   Lam absTy expr : absTy -> exprTy
  Lam abs expr
    -> do let newExprBindCtx = addBinding (absTy abs) exprBindCtx
          exprTy <- exprType newExprBindCtx typeBindCtx typeBindings typeCtx expr
          Right $ fixType $ Arrow (absTy abs) exprTy

  -- |
  --   f : a -> b    x : a
  -- -----------------------
  --       App f x : b
  App f x
    -> do fTy <- exprType exprBindCtx typeBindCtx typeBindings typeCtx f -- Both f and x must type check
          xTy <- exprType exprBindCtx typeBindCtx typeBindings typeCtx x

          resFTy <- maybe (Left $ EMsg $ text "Unknown named type in function application") Right $ _typeInfoType <$> resolveTypeInitialInfo fTy typeCtx

          let errAppMismatch = Left $ EAppMismatch resFTy xTy
          case unfixType resFTy of
            -- Regular function application attempt
            Arrow aTy bTy -> case typeEq typeBindCtx typeBindings typeCtx aTy xTy of
                                 Left err -> Left err
                                 Right isSameType
                                   | isSameType -> Right bTy
                                   | otherwise  -> Left $ EAppMismatch fTy xTy
            _ -> error "Attempting to apply non-arrow type"

  -- Provided an expression type checks and its type is in the correct place within a sum,
  -- has that sum type.
  Sum expr ix inTypr
    -> do -- Expression must type check
          exprTy <- exprType exprBindCtx typeBindCtx typeBindings typeCtx expr

          -- Expression must have the type of the index in the sum it claims to have...
          sumTy <- if NE.length inTypr < ix
                     then Left $ EMsg $ text "Can't type check a sum because the index is larger than the number of types in the sum"
                     else Right (inTypr NE.!! ix)

          _ <- case typeEq typeBindCtx typeBindings typeCtx exprTy sumTy of
                   Left err -> Left err
                   Right isSameType
                     | isSameType -> Right ()
                     | otherwise  -> Left $ ESumMismatch exprTy ix inTypr

          -- Allow the other types in the sum to not exist...
          _ <- Right ()

          -- Type is the claimed sum
          Right $ fixType $ SumT inTypr

  -- A product is typed by the order of each expression it contains
  Product prodExprs
    -> do -- type check each successive expression
          prodExprTys <- mapM (exprType exprBindCtx typeBindCtx typeBindings typeCtx) prodExprs

          -- the type is the product of those types
          Right $ fixType $ ProductT prodExprTys

  -- Provided an expression typechecks and its type exists within the union, it has the claimed union type.
  -- TODO: Unused types in the union are not themselves checked for consistency
  -- TODO: The same type appearing more than once should be an error?
  Union unionExpr unionTypeIndex unionTypes
    -> do -- type check injected expression
          exprTy <- exprType exprBindCtx typeBindCtx typeBindings typeCtx unionExpr

          -- Type must be what we claim it is...
          _ <- case typeEq typeBindCtx typeBindings typeCtx exprTy unionTypeIndex of
                   Right True  -> Right ()
                   Right False -> Left $ EMsg $ text "Expression doesnt have the type within the union it claims to have"
                   Left err    -> Left err

          -- Type must be in the set somewhere...
          _ <- if Set.member (Right True) . Set.map (typeEq typeBindCtx typeBindings typeCtx exprTy) $ unionTypes
                 then Right ()
                 else Left $ EMsg $ text "Expressions type is not within the union"

          -- the type is the claimed union
          Right $ fixType $ UnionT unionTypes

  -- | A binding is typed by the context
  -- It is assumed the exprBindCtx has been type checked
  --
  -- b : t IN exprBindCtx
  -- -----------------
  --      b : t
  Binding b
    -> case lookupBindingTy b exprBindCtx of
          Nothing -> Left $ EMsg $ text "Expression refers to a non-existant binding"
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
          scrutineeTy <- exprType exprBindCtx typeBindCtx typeBindings typeCtx $ _caseScrutinee c

          case _caseCaseBranches c of

            -- The case expression is then typed by the default branch, if its well typed
            DefaultOnly defExpr
              -> exprType exprBindCtx typeBindCtx typeBindings typeCtx defExpr

            CaseBranches (branch0 :| branches) mDefExpr
              -> do -- Check the all the branches
                    branch0Ty <- branchType branch0 scrutineeTy exprBindCtx typeBindCtx typeBindings typeCtx
                    branchTys <- mapM (\branch -> branchType branch scrutineeTy exprBindCtx typeBindCtx typeBindings typeCtx) branches

                    -- Check the default branch if it exists
                    mDefExprTy <- maybe (Right Nothing) (fmap Just . exprType exprBindCtx typeBindCtx typeBindings typeCtx) mDefExpr

                    -- If the default branch exists, its type must be the same as the first branch
                    _ <- maybe (Right ())
                               (\defExprTy -> case typeEq typeBindCtx typeBindings typeCtx defExprTy branch0Ty of
                                                  Left err -> Left err
                                                  Right isSameType
                                                    | isSameType -> Right ()
                                                    | otherwise  -> Left $ ECaseDefaultMismatch defExprTy branch0Ty
                               )
                               mDefExprTy

                    -- Any other branches must have the same type as the first
                    _ <- mapM (\branchTy -> case typeEq typeBindCtx typeBindings typeCtx branchTy branch0Ty of
                                                Left err -> Left err
                                                Right isSameType
                                                  | isSameType -> Right ()
                                                  | otherwise  -> Left $ EMsg $ text "Branch and first branch have different result types"
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
          exprTy <- exprType exprBindCtx newTypeBindCtx newTypeBindings typeCtx expr
          Right $ fixType $ BigArrow abs exprTy

  --    f : aKind BigLamArrow bTy      xTy :: aKind
  -- ---------------------------------------------------
  --              BigApp f xTy : bTy
  BigApp f x
    -> do xKy <- typeKind typeBindCtx typeCtx x
          let newTypeBindCtx  = addBinding xKy typeBindCtx
              newTypeBindings = bind x typeBindings

          -- Check f under x
          fTy <- exprType exprBindCtx newTypeBindCtx newTypeBindings typeCtx f

          -- TODO maybe verify the xTy we've been given to apply?

          resFTy <- maybe (Left $ EMsg $ text "Unknown named type in Big function application") Right $ _typeInfoType <$> resolveTypeInitialInfo fTy typeCtx

          case unfixType resFTy of
            -- Regular big application attempt
            BigArrow aKy bTy
              | aKy == xKy -> Right $ instantiate x bTy -- TODO: all bindings need to be instantiated
              | otherwise  -> Left $ EBigAppMismatch fTy xKy

            _ -> Left $ EMsg $ text "In big application, function must have a big arrow type"

-- Type check a case branch, requiring it match the expected type
-- , if so, type checking the result expression which is returned.
branchType
  :: ( BindAbs b abs tb
     , Binds tb Kind
     , Ord tb
     )
  => CaseBranch (Expr b abs tb) (MatchArg b tb)
  -> Type tb
  -> ExprBindCtx b tb
  -> TypeBindCtx tb
  -> TypeBindings tb
  -> TypeCtx tb
  -> Either (Error tb) (Type tb)
branchType (CaseBranch lhs rhs) expectedTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  bindings <- checkMatchWith lhs expectedTy exprBindCtx typeBindCtx typeBindings typeCtx
  exprType (addBindings bindings exprBindCtx) typeBindCtx typeBindings typeCtx rhs


-- | Check that a MatchArg matches the expected Type
-- If so, return a list of types of any bound bindings.
checkMatchWith
  :: (Binds b (Type tb),Binds tb Kind,Ord tb)
  => MatchArg b tb
  -> Type tb
  -> ExprBindCtx b tb
  -> TypeBindCtx tb
  -> TypeBindings tb
  -> TypeCtx tb
  -> Either (Error tb) [Type tb]
checkMatchWith match expectTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  rExpectTy <- maybe (Left $ EMsg $ text "The expected type in a pattern is a type name with no definition.") Right $ _typeInfoType <$> resolveTypeInitialInfo expectTy typeCtx
  case match of

    -- Bind the value
    Bind
      -> Right [expectTy]

    MatchBinding b
      -> do -- the type of the binding
            bTy <- maybe (Left $ EMsg $ text "pattern match on a non-existant binding") Right $ lookupBindingTy b exprBindCtx
            case typeEq typeBindCtx typeBindings typeCtx bTy expectTy of
                Left err         -> Left err
                Right isSameType -> if isSameType then pure [] else Left $ EMsg $ text "pattern match on a binding from a different type"

    MatchSum sumIndex nestedMatchArg
      -> do sumTypes <- case unfixType rExpectTy of
                      SumT sumTypes -> Right sumTypes
                      _             -> Left . EMsg . text $ "Expected sum type in pattern match"

            -- index must be within the number of alternative in the sum type
            matchedTy <- if NE.length sumTypes < sumIndex then Left $ EMsg $ text "Matching on a larger sum index than the sum type contains" else Right (sumTypes NE.!! sumIndex)

            -- must have the expected index type
            checkMatchWith nestedMatchArg matchedTy exprBindCtx typeBindCtx typeBindings typeCtx

    MatchProduct nestedMatchArgs
      -> do prodTypes <- case unfixType rExpectTy of
                             ProductT prodTypes
                               -> Right prodTypes
                             _ -> Left $ EMsg $ text "Expected product type in pattern match"

            checkMatchesWith nestedMatchArgs prodTypes exprBindCtx typeBindCtx typeBindings typeCtx

    MatchUnion unionIndexTy nestedMatchArg
      -> do unionTypes <- case unfixType rExpectTy of
                        UnionT unionTypes -> Right unionTypes
                        _                 -> Left $ EMsg $ text "Expected union type in pattern match"

            -- type index must be a member of the union alternatives
            _ <- if Set.member unionIndexTy unionTypes then Right () else Left $ EMsg $ text "Matching on a type which isnt a member of the union"

            -- must actually match on the expected type
            checkMatchWith nestedMatchArg unionIndexTy exprBindCtx typeBindCtx typeBindings typeCtx


checkMatchesWith
  :: (Binds b (Type tb),Binds tb Kind,Ord tb)
  => [MatchArg b tb]
  -> [Type tb]
  -> ExprBindCtx b tb
  -> TypeBindCtx tb
  -> TypeBindings tb
  -> TypeCtx tb
  -> Either (Error tb) [Type tb]
checkMatchesWith matches types exprBindCtx typeBindCtx typeBindings typeCtx = case (matches,types) of
  ([],[]) -> Right []
  ([],_)  -> Left $ EMsg $ text "Expected more patterns in match"
  (_,[])  -> Left $ EMsg $ text "Too many patterns in match"
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
appise [e]       = e
appise (e:e':es) = appise (fixExpr (App e e') : es)

lamise :: [abs] -> Expr b abs tb -> Expr b abs tb
lamise []        _ = error "Cant lamise empty list of abstractions"
lamise [t]       e = fixExpr $ Lam t e
lamise (t:t':ts) e = fixExpr $ Lam t (lamise (t':ts) e)

