{-# LANGUAGE
     ConstraintKinds
   , ConstraintKinds
   , DataKinds
   , DeriveAnyClass
   , EmptyCase
   , FlexibleContexts
   , FlexibleInstances
   , GADTs
   , LambdaCase
   , MultiParamTypeClasses
   , OverloadedStrings
   , PatternSynonyms
   , RankNTypes
   , ScopedTypeVariables
   , StandaloneDeriving
   , TypeFamilies
   , UndecidableInstances
   , TypeOperators
   #-}
{-|
Module      : PL.Expr
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

An AST containing anonymous functions, sums, products and union types.
Indexed by de bruijn indexes and with some level of type functions.

The underlying expression type is parameterised over:
- Itself for use in sub-expressions
- Tt's phases which is used in a 'trees-that-grow' style to allow:
  - Extending each constructor
  - Adding new constructors
  depending on the phase.

Patterns are provided for constructing and matching on 'default' expressions.
-}
module PL.Expr
  ( Expr
  , pattern Lam
  , pattern App
  , pattern Binding
  , pattern CaseAnalysis
  , pattern Sum
  , pattern Product
  , pattern Union
  , pattern BigLam
  , pattern BigApp
  , pattern ExprExtension

  , ExprFor
  , ExprF (..)

  , DefaultPhase

  , MatchArg (..)

  , mapSubExpressions
  , topExprType
  , exprType
  , branchType
  , checkMatchWith
  , checkMatchesWith

  , appise
  , lamise

  , LamExtension
  , AppExtension
  , BindingExtension
  , CaseAnalysisExtension
  , SumExtension
  , ProductExtension
  , UnionExtension
  , BigLamExtension
  , BigAppExtension
  , ExprExtension
  , BindingFor
  , AbstractionFor
  , TypeBindingFor

  )
  where

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

import PL.Var
import PL.TyVar

import Control.Applicative
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import Data.Void
import GHC.Types (Constraint)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | Expr is an 'ExprF' that:
-- - Uses itself for sub-expressions
-- - Uses types (without names) to abstract expressions in lambdas.
-- - Uses De Bruijn indexes for variable (and type variable) bindings I.E. a natural number which
--   points a number of lambda abstractions away.
type Expr = ExprFor DefaultPhase

-- | ExprFor is an 'ExprF' expression that uses itself as subexpressions.
type ExprFor phase = FixExpr phase ExprF

-- | A typed lambda calculus with anonymous sums, products, unions,
-- type level functions and case analysis.
--
-- 'phase' is a type index indicating the phase in which the expression
-- exists. For example, this could indicate an expression that:
-- - Has just been parsed and retains source code positions and comments
-- - Has been type checked successfully
-- - Has been reduced to its normal form
--
-- For now there is only one phase, the 'DefaultPhase'.
--
-- Internally, phases are also associated with:
-- - The type of abstractions used in Lambdas. In many languages this might be a
--   new variable name, perhaps with a type annotation.
--
-- - The type of bindings which refer to an abstraction.
--   In many languages, this might be a variable name "foo".
--
-- - The type of bindings used at the type level.
--
-- 'expr' is used in recursive positions where an expression contains a
-- subexpression. By wrapping ExprF with FixExpr we can recursivly pass an
-- expression type to itself. See 'ExprFor'.
--
--   The examples used in constructor comments use completly arbitrary syntax.
data ExprF phase expr

    -- | Lambda abstraction
    --
    -- Accepts some 'abs'tracted expr which is then bound under the resulting
    -- 'expr' and can be referenced by 'b'.
    --
    -- E.G.
    -- \(x::Int). x+x
    = LamF
      { _lamExtension :: LamExtension phase
      , _take         :: AbstractionFor phase
      , _expr         :: expr
      }

    -- | Application
    --
    -- Apply an expression to another.
    --
    -- E.G.
    -- (\x -> x) 1
    | AppF
      { _appExtension :: AppExtension phase
      , _f            :: expr
      , _x            :: expr
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
    | BindingF
      { _bindingExtension :: BindingExtension phase
      , _binding          :: BindingFor phase
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
    | CaseAnalysisF
      { _caseAnalysisExtension :: CaseAnalysisExtension phase
      , _caseAnalysis          :: Case expr (MatchArg (BindingFor phase) (TypeBindingFor phase))
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
    | SumF
      { _sumExtension :: SumExtension phase
      , _sumExpr      :: expr
      , _sumIndex     :: Int
      , _sumType      :: NonEmpty (Type (TypeBindingFor phase))
      }

    -- | An Product is many ordered expressions.
    --
    -- E.G.
    --
    -- (true, 1, 'a') :: (Bool, Int, Char)
    --
    -- Is a product of Bool, Int and Char.
    | ProductF
      { _productExtension :: ProductExtension phase
      , _prodExprs        :: [expr]
      }

    -- | A Union is an expression indexed within an unordered, unique collection
    -- of expression types by its type.
    --
    -- E.G.
    -- x :: Bool :: {Char, Bool, Int}
    --
    -- Is a variable with type Bool within a Union of Char,Bool and Int.
    | UnionF
      { _unionExtension :: UnionExtension phase
      , _unionExpr      :: expr
      , _unionTypeIndex :: Type (TypeBindingFor phase)
      , _unionType      :: Set.Set (Type (TypeBindingFor phase))
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
    | BigLamF
      { _bigLamExtension :: BigLamExtension phase
      , _takeTy          :: Kind -- TODO: Replace with 'tabs' mirroring 'abs'.
      , _expr            :: expr
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
    | BigAppF
      { _bigAppExtension :: BigAppExtension phase
      , _f               :: expr
      , _xTy             :: Type (TypeBindingFor phase)
      }

    | ExprExtensionF
      { _exprExtension :: !(ExprExtension phase)
      }

deriving instance
  (Eq (LamExtension phase)
  ,Eq (AppExtension phase)
  ,Eq (BindingExtension phase)
  ,Eq (CaseAnalysisExtension phase)
  ,Eq (SumExtension phase)
  ,Eq (ProductExtension phase)
  ,Eq (UnionExtension phase)
  ,Eq (BigLamExtension phase)
  ,Eq (BigAppExtension phase)
  ,Eq (ExprExtension phase)
  ,Eq (AbstractionFor phase)
  ,Eq (BindingFor phase)
  ,Eq (TypeBindingFor phase)
  ,Eq expr
  )
  => Eq (ExprF phase expr)

deriving instance
  (Show (LamExtension phase)
  ,Show (AppExtension phase)
  ,Show (BindingExtension phase)
  ,Show (CaseAnalysisExtension phase)
  ,Show (SumExtension phase)
  ,Show (ProductExtension phase)
  ,Show (UnionExtension phase)
  ,Show (BigLamExtension phase)
  ,Show (BigAppExtension phase)
  ,Show (ExprExtension phase)
  ,Show (AbstractionFor phase)
  ,Show (BindingFor phase)
  ,Show (TypeBindingFor phase)
  ,Show expr
  )
  => Show (ExprF phase expr)

-- The type families below allow adding new parameters to each of the
-- base constructors of an expression which depend upon the phase
type family LamExtension phase
type family AppExtension phase
type family BindingExtension phase
type family CaseAnalysisExtension phase
type family SumExtension phase
type family ProductExtension phase
type family UnionExtension phase
type family BigLamExtension phase
type family BigAppExtension phase

-- The ExprExtension type family allows adding new constructors to the base Expr
-- type which depend upon the phase
type family ExprExtension phase

type family BindingFor     phase
type family AbstractionFor phase
type family TypeBindingFor phase

-- Some patterns to make working with ExprF nicer
void :: Void
void = error "Cannot evaluate Void"

-- TODO: Could these use AbstractionFor, etc to be slightly more general and
-- still usable or should other phases define their own patterns, perhaps to be
-- imported qualified like "TypeChecked.Lam ABS EXPR"?

pattern Lam :: Type TyVar -> Expr -> Expr
pattern Lam abs expr <- FixExpr (LamF _ abs expr)
  where Lam abs expr =  FixExpr (LamF void abs expr)

pattern App :: Expr -> Expr -> Expr
pattern App f x <- FixExpr (AppF _ f x)
  where App f x =  FixExpr (AppF void f x)

pattern Binding :: Var -> Expr
pattern Binding b <- FixExpr (BindingF _ b)
  where Binding b = FixExpr (BindingF void b)

pattern CaseAnalysis :: Case Expr (MatchArg Var TyVar) -> Expr
pattern CaseAnalysis c <- FixExpr (CaseAnalysisF _ c)
  where CaseAnalysis c =  FixExpr (CaseAnalysisF void c)

pattern Sum :: Expr -> Int -> NonEmpty (Type TyVar) -> Expr
pattern Sum expr ix types <- FixExpr (SumF _ expr ix types)
  where Sum expr ix types =  FixExpr (SumF void expr ix types)

pattern Product :: [Expr] -> Expr
pattern Product exprs <- FixExpr (ProductF _ exprs)
  where Product exprs =  FixExpr (ProductF void exprs)

pattern Union :: Expr -> Type TyVar -> Set.Set (Type TyVar) -> Expr
pattern Union expr typeIx types <- FixExpr (UnionF _ expr typeIx types)
  where Union expr typeIx types =  FixExpr (UnionF void expr typeIx types)

pattern BigLam :: Kind -> Expr -> Expr
pattern BigLam typeAbs expr <- FixExpr (BigLamF _ typeAbs expr)
  where BigLam typeAbs expr =  FixExpr (BigLamF void typeAbs expr)

pattern BigApp :: Expr -> Type TyVar -> Expr
pattern BigApp f xType <- FixExpr (BigAppF _ f xType)
  where BigApp f xType =  FixExpr (BigAppF void f xType)

pattern ExprExtension :: Expr
pattern ExprExtension <- FixExpr (ExprExtensionF _)
  where ExprExtension =  FixExpr (ExprExtensionF void)

-- Phases
data DefaultPhase

-- The DefaultPhase has no extensions to constructors or the expression itself
-- Bindings are Var's (indexes without names), abstractions are Types (with no names) and type
-- bindings are TyVars (indexes without names).
type instance LamExtension DefaultPhase = Void
type instance AppExtension DefaultPhase = Void
type instance BindingExtension DefaultPhase = Void
type instance CaseAnalysisExtension DefaultPhase = Void
type instance SumExtension DefaultPhase = Void
type instance ProductExtension DefaultPhase = Void
type instance UnionExtension DefaultPhase = Void
type instance BigLamExtension DefaultPhase = Void
type instance BigAppExtension DefaultPhase = Void

-- TODO: Should _this_ be unit instead of void?
type instance ExprExtension DefaultPhase = Void

type instance BindingFor     DefaultPhase = Var
type instance AbstractionFor DefaultPhase = Type TyVar
type instance TypeBindingFor DefaultPhase = TyVar

-- TODO: Recursion schemes can probably be used for these instances now.
-- They're also not as generic as they could be.

-- An Expr abstracts over itself
instance HasAbs Expr where
  applyToAbs f (Lam abs e) = Lam abs (f e)
  applyToAbs _ e           = e

-- An Expr binds with Vars
instance HasBinding Expr Var where
  applyToBinding f (Binding e) = Binding (f e)
  applyToBinding _ e           = e

-- An Expr contains NON-abstracted sub-expressions
instance HasNonAbs Expr where
  applyToNonAbs f (App x y)                 = App (f x) (f y)
  applyToNonAbs f (CaseAnalysis c)          = CaseAnalysis (mapCaseExpr (applyToNonAbs f) c)
  applyToNonAbs f (Sum expr ix ty)          = Sum (f expr) ix ty
  applyToNonAbs f (Product prodExprs)       = Product (map f prodExprs)
  applyToNonAbs f (Union unionExpr tyIx ty) = Union (f unionExpr) tyIx ty
  applyToNonAbs f (BigLam takeTy expr)      = BigLam takeTy (f expr)
  applyToNonAbs f (BigApp fExpr xTy)        = BigApp (f fExpr) xTy
  applyToNonAbs f e                         = e

-- Map a function over all contained subexpressions.
-- The function should preserve the type of the expression.
mapSubExpressions
  :: (Expr -> Expr)
  -> Expr
  -> Expr
mapSubExpressions f e = case e of
  Lam ty expr
    -> Lam ty (f expr)

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
  e -> e

-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArg b tb
  = MatchSum
      { _index :: Int           -- ^ The index of the type within the sum we wish to match
      , _match :: MatchArg b tb -- ^ Match within the sum
      }

  | MatchProduct
      { _matches :: [MatchArg b tb] -- ^ Match against each of the products values
      }

  | MatchUnion
      { _typeIndex :: Type tb       -- ^ The index of the type within the union we weish to match
      , _match     :: MatchArg b tb -- ^ Match within the union
      }

  | MatchBinding
      { _equalTo :: b -- ^ The value should match the value of the binding
      }

  | Bind -- ^ Match anything and bind it
  deriving (Show,Eq)

-- | A top-level expression is an expression without a bindings context.
topExprType
  :: TypeCtx TyVar
  -> Expr
  -> Either (Error TyVar) (Type TyVar)
topExprType = exprType emptyCtx emptyCtx emptyBindings

-- | Under a binding context, type check an expression.
exprType
  :: BindCtx Var (Type TyVar) -- Associate expr bindings to their types
  -> BindCtx TyVar Kind       -- Associate type bindings to their Kinds
  -> Bindings (Type TyVar)    -- Associate type bindings to their bound or unbound types
  -> TypeCtx TyVar            -- Associate Named types to their TypeInfo
  -> Expr                     -- Expression to type-check
  -> Either (Error TyVar) (Type TyVar)
exprType exprBindCtx typeBindCtx typeBindings typeCtx e = case e of

  -- ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     expr : exprTy
  -- ----------------------------------
  --   Lam absTy expr : absTy -> exprTy
  Lam abs expr
    -> do let newExprBindCtx = addBinding (absTy abs) exprBindCtx
          exprTy <- exprType newExprBindCtx typeBindCtx typeBindings typeCtx expr
          Right $ fixType $ Arrow (absTy abs) exprTy

  --
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
          sumTy <- if  NE.length inTypr <= ix
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

  -- A binding is typed by the context
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

  e -> error "Non-exhaustive pattern in type check"

-- Type check a case branch, requiring it match the expected type
-- , if so, type checking the result expression which is returned.
branchType
  :: CaseBranch Expr (MatchArg Var TyVar)
  -> Type TyVar
  -> BindCtx Var (Type TyVar)
  -> BindCtx TyVar Kind
  -> Bindings (Type TyVar)
  -> TypeCtx TyVar
  -> Either (Error TyVar) (Type TyVar)
branchType (CaseBranch lhs rhs) expectedTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  bindings <- checkMatchWith lhs expectedTy exprBindCtx typeBindCtx typeBindings typeCtx
  exprType (addBindings bindings exprBindCtx) typeBindCtx typeBindings typeCtx rhs


-- | Check that a MatchArg matches the expected Type
-- If so, return a list of types of any bound bindings.
checkMatchWith
  :: MatchArg Var TyVar
  -> Type TyVar
  -> BindCtx Var (Type TyVar)
  -> BindCtx TyVar Kind
  -> Bindings (Type TyVar)
  -> TypeCtx TyVar
  -> Either (Error TyVar) [Type TyVar]
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
            matchedTy <- if NE.length sumTypes <= sumIndex
                           then Left $ EMsg $ text "Matching on a larger sum index than the sum type contains"
                           else Right (sumTypes NE.!! sumIndex)

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
  :: [MatchArg Var TyVar]
  -> [Type TyVar]
  -> BindCtx Var (Type TyVar)
  -> BindCtx TyVar Kind
  -> Bindings (Type TyVar)
  -> TypeCtx TyVar
  -> Either (Error TyVar) [Type TyVar]
checkMatchesWith matches types exprBindCtx typeBindCtx typeBindings typeCtx = case (matches,types) of
  ([],[]) -> Right []
  ([],_)  -> Left $ EMsg $ text "Expected more patterns in match"
  (_,[])  -> Left $ EMsg $ text "Too many patterns in match"
  (m:ms,t:ts)
    -> checkMatchWith m t exprBindCtx typeBindCtx typeBindings typeCtx >>= \boundTs -> checkMatchesWith ms ts exprBindCtx typeBindCtx typeBindings typeCtx >>= Right . (boundTs ++)

appise :: [Expr] -> Expr
appise []        = error "Cant appise empty list of expressions"
appise [e]       = e
appise (e:e':es) = appise (App e e' : es)

lamise :: [Type TyVar] -> Expr -> Expr
lamise []        _ = error "Cant lamise empty list of abstractions"
lamise [t]       e = Lam t e
lamise (t:t':ts) e = Lam t (lamise (t':ts) e)
