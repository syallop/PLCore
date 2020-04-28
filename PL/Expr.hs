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
  , pattern LamExt
  , pattern App
  , pattern AppExt
  , pattern Binding
  , pattern BindingExt
  , pattern CaseAnalysis
  , pattern CaseAnalysisExt
  , pattern Sum
  , pattern SumExt
  , pattern Product
  , pattern ProductExt
  , pattern EmptyProduct
  , pattern EmptyProductExt
  , pattern Union
  , pattern UnionExt
  , pattern BigLam
  , pattern BigLamExt
  , pattern BigApp
  , pattern BigAppExt
  , pattern ExprExtension
  , pattern ExprExtensionExt

  , ExprFor
  , ExprF (..)

  , DefaultPhase

  , MatchArg (..)
  , pattern MatchSum
  , pattern MatchSumExt
  , pattern MatchProduct
  , pattern MatchProductExt
  , pattern MatchEmptyProduct
  , pattern MatchEmptyProductExt
  , pattern MatchUnion
  , pattern MatchUnionExt
  , pattern MatchBinding
  , pattern MatchBindingExt
  , pattern Bind
  , pattern BindExt
  , pattern MatchArgExtension
  , pattern MatchArgExtensionExt

  , MatchArgFor
  , MatchArgF (..)

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

  , MatchSumExtension
  , MatchProductExtension
  , MatchUnionExtension
  , MatchBindingExtension
  , BindExtension
  , MatchArgExtension
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.ExprLike
import PL.FixPhase
import PL.Kind
import PL.Name
import PLPrinter
import PLPrinter.Doc
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.TypeCtx

import PL.Var
import PL.TyVar

import Control.Applicative
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
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
type ExprFor phase = FixPhase phase ExprF

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
-- subexpression. By wrapping ExprF with FixPhase we can recursivly pass an
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
      , _caseAnalysis          :: Case expr (MatchArgFor phase)
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
      , _sumType      :: NonEmpty (TypeFor phase)
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
      , _unionTypeIndex :: TypeFor phase
      , _unionType      :: Set.Set (TypeFor phase)
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
      , _xTy             :: TypeFor phase
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
  ,Eq (TypeFor phase)
  ,Eq (MatchArgFor phase)
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
  ,Show (TypeFor phase)
  ,Show (MatchArgFor phase)
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

-- LamF for phases where there is no extension to the constructor.
pattern Lam :: LamExtension phase ~ Void => AbstractionFor phase -> ExprFor phase -> ExprFor phase
pattern Lam abs expr <- FixPhase (LamF _ abs expr)
  where Lam abs expr =  FixPhase (LamF void abs expr)

pattern LamExt :: LamExtension phase -> AbstractionFor phase -> ExprFor phase -> ExprFor phase
pattern LamExt ext abs expr <- FixPhase (LamF ext abs expr)
  where LamExt ext abs expr =  FixPhase (LamF ext abs expr)

-- AppF for phases where there is no extension to the constructor.
pattern App :: AppExtension phase ~ Void => ExprFor phase -> ExprFor phase -> ExprFor phase
pattern App f x <- FixPhase (AppF _ f x)
  where App f x =  FixPhase (AppF void f x)

pattern AppExt :: AppExtension phase -> ExprFor phase -> ExprFor phase -> ExprFor phase
pattern AppExt ext f x <- FixPhase (AppF ext f x)
  where AppExt ext f x =  FixPhase (AppF ext f x)

-- BindingF for phases where there is no extension to the constructor.
pattern Binding :: BindingExtension phase ~ Void => BindingFor phase -> ExprFor phase
pattern Binding b <- FixPhase (BindingF _ b)
  where Binding b = FixPhase (BindingF void b)

pattern BindingExt :: BindingExtension phase -> BindingFor phase -> ExprFor phase
pattern BindingExt ext b <- FixPhase (BindingF ext b)
  where BindingExt ext b = FixPhase (BindingF ext b)

-- CaseAnalysisF for phases where there is no extension to the constructor.
pattern CaseAnalysis :: CaseAnalysisExtension phase ~ Void => Case (ExprFor phase) (MatchArgFor phase) -> ExprFor phase
pattern CaseAnalysis c <- FixPhase (CaseAnalysisF _ c)
  where CaseAnalysis c =  FixPhase (CaseAnalysisF void c)

pattern CaseAnalysisExt :: CaseAnalysisExtension phase -> Case (ExprFor phase) (MatchArgFor phase) -> ExprFor phase
pattern CaseAnalysisExt ext c <- FixPhase (CaseAnalysisF ext c)
  where CaseAnalysisExt ext c =  FixPhase (CaseAnalysisF ext c)

-- SumF for phases where there is no extension to the constructor.
pattern Sum :: SumExtension phase ~ Void =>  ExprFor phase -> Int -> NonEmpty (TypeFor phase) -> ExprFor phase
pattern Sum expr ix types <- FixPhase (SumF _ expr ix types)
  where Sum expr ix types =  FixPhase (SumF void expr ix types)

pattern SumExt :: SumExtension phase -> ExprFor phase -> Int -> NonEmpty (TypeFor phase) -> ExprFor phase
pattern SumExt ext expr ix types <- FixPhase (SumF ext expr ix types)
  where SumExt ext expr ix types =  FixPhase (SumF ext expr ix types)

-- ProductF for phases where there is no extension to the constructor.
pattern Product :: ProductExtension phase ~ Void =>  [ExprFor phase] -> ExprFor phase
pattern Product exprs <- FixPhase (ProductF _ exprs)
  where Product exprs =  FixPhase (ProductF void exprs)

-- The empty product for phases where there is no extension to the constructor.
pattern EmptyProduct :: ProductExtension phase ~ Void => ExprFor phase
pattern EmptyProduct <- FixPhase (ProductF _ [])
  where EmptyProduct =  FixPhase (ProductF void [])

pattern ProductExt :: ProductExtension phase ->  [ExprFor phase] -> ExprFor phase
pattern ProductExt ext exprs <- FixPhase (ProductF ext exprs)
  where ProductExt ext exprs =  FixPhase (ProductF ext exprs)

pattern EmptyProductExt :: ProductExtension phase -> ExprFor phase
pattern EmptyProductExt ext <- FixPhase (ProductF ext [])
  where EmptyProductExt ext =  FixPhase (ProductF ext [])

-- UnionF for phases where there is no extension to the constructor.
pattern Union :: UnionExtension phase ~ Void =>  ExprFor phase -> TypeFor phase -> Set.Set (TypeFor phase) -> ExprFor phase
pattern Union expr typeIx types <- FixPhase (UnionF _ expr typeIx types)
  where Union expr typeIx types =  FixPhase (UnionF void expr typeIx types)

pattern UnionExt :: UnionExtension phase ->  ExprFor phase -> TypeFor phase -> Set.Set (TypeFor phase) -> ExprFor phase
pattern UnionExt ext expr typeIx types <- FixPhase (UnionF ext expr typeIx types)
  where UnionExt ext expr typeIx types =  FixPhase (UnionF ext expr typeIx types)

-- BigLamF for phases where there is no extension to the constructor.
pattern BigLam :: BigLamExtension phase ~ Void =>  Kind -> ExprFor phase -> ExprFor phase
pattern BigLam typeAbs expr <- FixPhase (BigLamF _ typeAbs expr)
  where BigLam typeAbs expr =  FixPhase (BigLamF void typeAbs expr)

pattern BigLamExt :: BigLamExtension phase ->  Kind -> ExprFor phase -> ExprFor phase
pattern BigLamExt ext typeAbs expr <- FixPhase (BigLamF ext typeAbs expr)
  where BigLamExt ext typeAbs expr =  FixPhase (BigLamF ext typeAbs expr)

-- BigAppF for phases where there is no extension to the constructor.
pattern BigApp :: BigAppExtension phase ~ Void =>  ExprFor phase -> TypeFor phase -> ExprFor phase
pattern BigApp f xType <- FixPhase (BigAppF _ f xType)
  where BigApp f xType =  FixPhase (BigAppF void f xType)

pattern BigAppExt :: BigAppExtension phase ->  ExprFor phase -> TypeFor phase -> ExprFor phase
pattern BigAppExt ext f xType <- FixPhase (BigAppF ext f xType)
  where BigAppExt ext f xType =  FixPhase (BigAppF ext f xType)

-- ExprExtensionF for phases where there is no extension to the number of constructors.
pattern ExprExtension :: ExprExtension phase ~ Void =>  ExprFor phase
pattern ExprExtension <- FixPhase (ExprExtensionF _)
  where ExprExtension =  FixPhase (ExprExtensionF void)

pattern ExprExtensionExt :: ExprExtension phase ->  ExprFor phase
pattern ExprExtensionExt ext <- FixPhase (ExprExtensionF ext)
  where ExprExtensionExt ext =  FixPhase (ExprExtensionF ext)

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
type instance AbstractionFor DefaultPhase = Type

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


type MatchArg = MatchArgFor DefaultPhase

-- TODO: Merge Fix types?
type MatchArgFor phase = FixPhase phase MatchArgF

-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArgF phase match
  = MatchSumF
      { _matchSumExtension :: MatchSumExtension phase
      , _index             :: Int   -- ^ The index of the type within the sum we wish to match
      , _match             :: match -- ^ Match within the sum
      }

  | MatchProductF
      { _matchProductExtension :: MatchProductExtension phase
      , _matches               :: [match] -- ^ Match against each of the products values
      }

  | MatchUnionF
      { _matchUnionExtension :: MatchUnionExtension phase
      , _typeIndex           :: TypeFor phase  -- ^ The index of the type within the union we weish to match
      , _match               :: match          -- ^ Match within the union
      }

  | MatchBindingF
      { _matchBinding :: MatchBindingExtension phase
      , _equalTo      :: BindingFor phase -- ^ The value should match the value of the binding
      }

  | BindF
      { _bindExtension :: BindExtension phase
      }
    -- ^ Match anything and bind it

  | MatchArgExtensionF
      { _matchArgExtension :: !(MatchArgExtension phase)
      }

deriving instance
  (Show (MatchSumExtension phase)
  ,Show (MatchProductExtension phase)
  ,Show (MatchUnionExtension phase)
  ,Show (MatchBindingExtension phase)
  ,Show (BindExtension phase)
  ,Show (MatchArgExtension phase)
  ,Show (TypeFor phase)
  ,Show (BindingFor phase)
  ,Show match
  )
  => Show (MatchArgF phase match)

deriving instance
  (Eq (MatchSumExtension phase)
  ,Eq (MatchProductExtension phase)
  ,Eq (MatchUnionExtension phase)
  ,Eq (MatchBindingExtension phase)
  ,Eq (BindExtension phase)
  ,Eq (MatchArgExtension phase)
  ,Eq (TypeFor phase)
  ,Eq (BindingFor phase)
  ,Eq match
  )
  => Eq (MatchArgF phase match)

-- The type families below allow adding new parameters to each of the
-- base constructors of an expression which depend upon the phase
type family MatchSumExtension phase
type family MatchProductExtension phase
type family MatchUnionExtension phase
type family MatchBindingExtension phase
type family BindExtension phase

-- The MatchArgExtension type family allows adding new constructors to the base
-- MatchArg type which depend upon the phase
type family MatchArgExtension phase

-- MatchSumF for phases where there is no extension to the constructor.
pattern MatchSum :: MatchSumExtension phase ~ Void => Int -> MatchArgFor phase -> MatchArgFor phase
pattern MatchSum ix match <- FixPhase (MatchSumF _ ix match)
  where MatchSum ix match =  FixPhase (MatchSumF void ix match)

pattern MatchSumExt :: MatchSumExtension phase -> Int -> MatchArgFor phase -> MatchArgFor phase
pattern MatchSumExt ext ix match <- FixPhase (MatchSumF ext ix match)
  where MatchSumExt ext ix match =  FixPhase (MatchSumF ext ix match)

-- MatchProductF for phases where there is no extension to the constructor.
pattern MatchProduct :: MatchProductExtension phase ~ Void => [MatchArgFor phase] -> MatchArgFor phase
pattern MatchProduct matches <- FixPhase (MatchProductF _ matches)
  where MatchProduct matches =  FixPhase (MatchProductF void matches)

-- MatchProductF for the empty product in phase with no extension to the
-- constructor.
pattern MatchEmptyProduct :: MatchProductExtension phase ~ Void => MatchArgFor phase
pattern MatchEmptyProduct <- FixPhase (MatchProductF _ [])
  where MatchEmptyProduct =  FixPhase (MatchProductF void [])

pattern MatchProductExt :: MatchProductExtension phase -> [MatchArgFor phase] -> MatchArgFor phase
pattern MatchProductExt ext matches <- FixPhase (MatchProductF ext matches)
  where MatchProductExt ext matches =  FixPhase (MatchProductF ext matches)

pattern MatchEmptyProductExt :: MatchProductExtension phase -> MatchArgFor phase
pattern MatchEmptyProductExt ext <- FixPhase (MatchProductF ext [])
  where MatchEmptyProductExt ext =  FixPhase (MatchProductF ext [])

-- MatchUnionF for phases where there is no extension to the constructor.
pattern MatchUnion :: MatchUnionExtension phase ~ Void => TypeFor phase -> MatchArgFor phase -> MatchArgFor phase
pattern MatchUnion typeIx match <- FixPhase (MatchUnionF _ typeIx match)
  where MatchUnion typeIx match =  FixPhase (MatchUnionF void typeIx match)

pattern MatchUnionExt :: MatchUnionExtension phase -> TypeFor phase -> MatchArgFor phase -> MatchArgFor phase
pattern MatchUnionExt ext typeIx match <- FixPhase (MatchUnionF ext typeIx match)
  where MatchUnionExt ext typeIx match =  FixPhase (MatchUnionF ext typeIx match)

-- MatchBindingF for phases where there is no extension to the constructor.
pattern MatchBinding :: MatchBindingExtension phase ~ Void => BindingFor phase -> MatchArgFor phase
pattern MatchBinding equalTo <- FixPhase (MatchBindingF _ equalTo)
  where MatchBinding equalTo =  FixPhase (MatchBindingF void equalTo)

pattern MatchBindingExt :: MatchBindingExtension phase -> BindingFor phase -> MatchArgFor phase
pattern MatchBindingExt ext equalTo <- FixPhase (MatchBindingF ext equalTo)
  where MatchBindingExt ext equalTo =  FixPhase (MatchBindingF ext equalTo)

-- BindF for phases where there is no extension to the constructor.
pattern Bind :: BindExtension phase ~ Void => MatchArgFor phase
pattern Bind <- FixPhase (BindF _)
  where Bind =  FixPhase (BindF void)

pattern BindExt :: BindExtension phase -> MatchArgFor phase
pattern BindExt ext <- FixPhase (BindF ext)
  where BindExt ext =  FixPhase (BindF ext)

-- MatchArgExtensionF for phases where there is no extension to the number of constructors.
pattern MatchArgExtension :: MatchArgExtension phase ~ Void => MatchArgFor phase
pattern MatchArgExtension <- FixPhase (MatchArgExtensionF _)
  where MatchArgExtension =  FixPhase (MatchArgExtensionF void)

pattern MatchArgExtensionExt :: MatchArgExtension phase -> MatchArgFor phase
pattern MatchArgExtensionExt ext <- FixPhase (MatchArgExtensionF ext)
  where MatchArgExtensionExt ext =  FixPhase (MatchArgExtensionF ext)

-- The DefaultPhase has no extensions to constructors or the MatchArg itself
type instance MatchSumExtension DefaultPhase = Void
type instance MatchProductExtension DefaultPhase = Void
type instance MatchUnionExtension DefaultPhase = Void
type instance MatchBindingExtension DefaultPhase = Void
type instance BindExtension DefaultPhase = Void

-- TODO: Should _this_ be unit instead of void?
type instance MatchArgExtension DefaultPhase = Void

-- | A top-level expression is an expression without a bindings context.
topExprType
  :: TypeCtx DefaultPhase
  -> Expr
  -> Either (Error DefaultPhase) Type
topExprType = exprType emptyCtx emptyCtx emptyBindings

-- | Under a binding context, type check an expression.
exprType
  :: BindCtx Var Type         -- Associate expr bindings to their types
  -> BindCtx TyVar Kind       -- Associate type bindings to their Kinds
  -> Bindings Type            -- Associate type bindings to their bound or unbound types
  -> TypeCtx DefaultPhase -- Associate Named types to their TypeInfo
  -> Expr                     -- Expression to type-check
  -> Either (Error DefaultPhase) Type
exprType exprBindCtx typeBindCtx typeBindings typeCtx e = case e of

  -- ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     expr : exprTy
  -- ----------------------------------
  --   Lam absTy expr : absTy -> exprTy
  Lam abs expr
    -> do let newExprBindCtx = addBinding abs exprBindCtx
          exprTy <- exprType newExprBindCtx typeBindCtx typeBindings typeCtx expr
          Right $ Arrow abs exprTy

  --
  --   f : a -> b    x : a
  -- -----------------------
  --       App f x : b
  App f x
    -> do fTy <- exprType exprBindCtx typeBindCtx typeBindings typeCtx f -- Both f and x must type check
          xTy <- exprType exprBindCtx typeBindCtx typeBindings typeCtx x

          resFTy <- maybe (Left $ EMsg $ text "Unknown named type in function application") Right $ _typeInfoType <$> resolveTypeInitialInfo fTy typeCtx

          let errAppMismatch = Left $ EAppMismatch resFTy xTy
          case resFTy of
            -- Regular function application attempt
            Arrow aTy bTy -> case typeEq typeBindCtx typeBindings typeCtx aTy xTy of
                                 Left err -> Left err
                                 Right isSameType
                                   | isSameType -> Right bTy
                                   | otherwise  -> Left $ EAppMismatch fTy xTy
            _ -> Left $ EMsg $ text "The first argument to an application must be a lambda with an arrow type."

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
          Right $ SumT inTypr

  -- A product is typed by the order of each expression it contains
  Product prodExprs
    -> do -- type check each successive expression
          prodExprTys <- mapM (exprType exprBindCtx typeBindCtx typeBindings typeCtx) prodExprs

          -- the type is the product of those types
          Right $ ProductT prodExprTys

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
          Right $ UnionT unionTypes

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
          Right $ BigArrow abs exprTy

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

          case resFTy of
            -- Regular big application attempt
            BigArrow aKy bTy
              | aKy == xKy -> Right $ instantiate x bTy -- TODO: all bindings need to be instantiated
              | otherwise  -> Left $ EBigAppMismatch fTy xKy

            _ -> Left $ EMsg $ text "In big application, function must have a big arrow type"

  e -> error "Non-exhaustive pattern in type check"

-- Type check a case branch, requiring it match the expected type
-- , if so, type checking the result expression which is returned.
branchType
  :: CaseBranch Expr MatchArg
  -> Type
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error DefaultPhase) Type
branchType (CaseBranch lhs rhs) expectedTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  bindings <- checkMatchWith lhs expectedTy exprBindCtx typeBindCtx typeBindings typeCtx
  exprType (addBindings bindings exprBindCtx) typeBindCtx typeBindings typeCtx rhs


-- | Check that a MatchArg matches the expected Type
-- If so, return a list of types of any bound bindings.
checkMatchWith
  :: MatchArg
  -> Type
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error DefaultPhase) [Type]
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
      -> do sumTypes <- case rExpectTy of
                      SumT sumTypes -> Right sumTypes
                      _             -> Left . EMsg . text $ "Expected sum type in pattern match"

            -- index must be within the number of alternative in the sum type
            matchedTy <- if NE.length sumTypes <= sumIndex
                           then Left $ EMsg $ text "Matching on a larger sum index than the sum type contains"
                           else Right (sumTypes NE.!! sumIndex)

            -- must have the expected index type
            checkMatchWith nestedMatchArg matchedTy exprBindCtx typeBindCtx typeBindings typeCtx

    MatchProduct nestedMatchArgs
      -> do prodTypes <- case rExpectTy of
                             ProductT prodTypes
                               -> Right prodTypes
                             _ -> Left $ EMsg $ text "Expected product type in pattern match"

            checkMatchesWith nestedMatchArgs prodTypes exprBindCtx typeBindCtx typeBindings typeCtx

    MatchUnion unionIndexTy nestedMatchArg
      -> do unionTypes <- case rExpectTy of
                        UnionT unionTypes -> Right unionTypes
                        _                 -> Left $ EMsg $ text "Expected union type in pattern match"

            -- type index must be a member of the union alternatives
            _ <- if Set.member unionIndexTy unionTypes then Right () else Left $ EMsg $ text "Matching on a type which isnt a member of the union"

            -- must actually match on the expected type
            checkMatchWith nestedMatchArg unionIndexTy exprBindCtx typeBindCtx typeBindings typeCtx

    _ -> error "Non-exhaustive pattern match when checking match statement"

checkMatchesWith
  :: [MatchArg]
  -> [Type]
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error DefaultPhase) [Type]
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

lamise :: [Type] -> Expr -> Expr
lamise []        _ = error "Cant lamise empty list of abstractions"
lamise [t]       e = Lam t e
lamise (t:t':ts) e = Lam t (lamise (t':ts) e)
