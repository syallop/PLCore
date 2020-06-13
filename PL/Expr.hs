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
  , pattern ContentBinding
  , pattern ContentBindingExt
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

  , mapSubExpressions
  , exprType
  , branchType

  , gatherContentNames
  , gatherExprsTypeContentNames

  , appise
  , lamise

  , LamExtension
  , AppExtension
  , BindingExtension
  , ContentBindingExtension
  , CaseAnalysisExtension
  , SumExtension
  , ProductExtension
  , UnionExtension
  , BigLamExtension
  , BigAppExtension
  , ExprExtension
  , BindingFor
  , ContentBindingFor
  , AbstractionFor
  , TypeBindingFor
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Hash
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
import PL.ReduceType
import PL.TyVar
import PL.Pattern
import PL.TypeCheck

import Control.Applicative
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import GHC.Types (Constraint)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Set (Set)

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

    -- | Bind an expression uniquely named by its content.
    --
    -- E.G. sha512/BGpbCZNFqE6aQE1
    --
    -- These hashes are (currently) non-cyclic and there is no way for an
    -- expression to refer to itself meaning this does not introduce
    -- recursion.
    | ContentBindingF
      { _nameBindingExtension :: ContentBindingExtension phase
      , _name                 :: ContentBindingFor phase
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
      , _caseAnalysis          :: Case expr (PatternFor phase)
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
  ,Eq (ContentBindingExtension phase)
  ,Eq (CaseAnalysisExtension phase)
  ,Eq (SumExtension phase)
  ,Eq (ProductExtension phase)
  ,Eq (UnionExtension phase)
  ,Eq (BigLamExtension phase)
  ,Eq (BigAppExtension phase)
  ,Eq (ExprExtension phase)
  ,Eq (AbstractionFor phase)
  ,Eq (BindingFor phase)
  ,Eq (ContentBindingFor phase)
  ,Eq (TypeBindingFor phase)
  ,Eq (TypeFor phase)
  ,Eq (PatternFor phase)
  ,Eq expr
  )
  => Eq (ExprF phase expr)

deriving instance
  (Ord (LamExtension phase)
  ,Ord (AppExtension phase)
  ,Ord (BindingExtension phase)
  ,Ord (ContentBindingExtension phase)
  ,Ord (CaseAnalysisExtension phase)
  ,Ord (SumExtension phase)
  ,Ord (ProductExtension phase)
  ,Ord (UnionExtension phase)
  ,Ord (BigLamExtension phase)
  ,Ord (BigAppExtension phase)
  ,Ord (ExprExtension phase)
  ,Ord (AbstractionFor phase)
  ,Ord (BindingFor phase)
  ,Ord (ContentBindingFor phase)
  ,Ord (TypeBindingFor phase)
  ,Ord (TypeFor phase)
  ,Ord (PatternFor phase)
  ,Ord (Case expr (PatternFor phase))
  ,Ord expr
  )
  => Ord (ExprF phase expr)

instance
  (Show (LamExtension phase)
  ,Show (AppExtension phase)
  ,Show (BindingExtension phase)
  ,Show (ContentBindingExtension phase)
  ,Show (CaseAnalysisExtension phase)
  ,Show (SumExtension phase)
  ,Show (ProductExtension phase)
  ,Show (UnionExtension phase)
  ,Show (BigLamExtension phase)
  ,Show (BigAppExtension phase)
  ,Show (ExprExtension phase)
  ,Show (AbstractionFor phase)
  ,Show (BindingFor phase)
  ,Show (ContentBindingFor phase)
  ,Show (TypeBindingFor phase)
  ,Show (TypeFor phase)
  ,Show (PatternFor phase)
  ,Show expr
  )
  => Show (ExprF phase expr) where
  show e = mconcat $ case e of
    LamF ext take expr
      -> ["{Lam ", show ext, " ", show take, " ", show expr, "}"]

    AppF ext f x
      -> ["{App ", show ext, " ", show f, " ", show x, "}"]

    BindingF ext b
      -> ["{Binding ", show ext, " ", show b, "}"]

    ContentBindingF ext name
      -> ["{ContentBinding ", show ext, " ", show name, "}"]

    CaseAnalysisF ext c
      -> ["{CaseAnalysis ", show ext, " ", show c, "}"]

    SumF ext x ix ty
      -> ["{Sum ", show ext, " ", show x, " ", show ix, " ", show ty, "}"]

    ProductF ext exprs
      -> ["{Product ", show ext, " ", show exprs, "}"]

    UnionF ext e tyIx ty
      -> ["{Union ", show ext, " ", show e, " ", show tyIx, " ", show ty , "}"]

    BigLamF ext ty expr
      -> ["{BigLam ", show ext, " ", show ty, " ", show expr, "}"]

    BigAppF ext f xTy
      -> ["{BigApp ", show ext, " ", show f, " ", show xTy, "}"]

    ExprExtensionF ext
      -> ["{ExprExtension ", show ext, "}"]

instance
  (Hashable (LamExtension phase)
  ,Hashable (AppExtension phase)
  ,Hashable (BindingExtension phase)
  ,Hashable (ContentBindingExtension phase)
  ,Hashable (CaseAnalysisExtension phase)
  ,Hashable (SumExtension phase)
  ,Hashable (ProductExtension phase)
  ,Hashable (UnionExtension phase)
  ,Hashable (BigLamExtension phase)
  ,Hashable (BigAppExtension phase)
  ,Hashable (ExprExtension phase)
  ,Hashable (AbstractionFor phase)
  ,Hashable (BindingFor phase)
  ,Hashable (ContentBindingFor phase)
  ,Hashable (TypeBindingFor phase)
  ,Hashable (TypeFor phase)
  ,Hashable (PatternFor phase)
  ,Hashable expr
  )
  => Hashable (ExprF phase expr) where
  toHashToken = \case
    LamF ext take expr
      -> HashTag "Expr.Lam" [toHashToken ext,toHashToken take,toHashToken expr]

    AppF ext f x
      -> HashTag "Expr.App" [toHashToken ext,toHashToken f,toHashToken x]

    BindingF ext b
      -> HashTag "Expr.Binding" [toHashToken ext,toHashToken b]

    ContentBindingF _ext c
      -> toHashToken c

    CaseAnalysisF ext c
      -> HashTag "Expr.Case" [toHashToken ext,toHashToken c]

    SumF ext x ix ty
      -> HashTag "Expr.Sum" [toHashToken ext,toHashToken x,HashInt ix,toHashToken ty]

    ProductF ext exprs
      -> HashTag "Expr.Product" [toHashToken ext,toHashToken exprs]

    UnionF ext e tyIx ty
      -> HashTag "Expr.Union" [toHashToken ext,toHashToken e,toHashToken tyIx,toHashToken ty]

    BigLamF ext ty expr
      -> HashTag "Expr.BigLam" [toHashToken ext,toHashToken ty,toHashToken expr]

    BigAppF ext f xTy
      -> HashTag "Expr.App" [toHashToken ext,toHashToken f,toHashToken xTy]

    ExprExtensionF ext
      -> HashTag "Expr.Extension" [toHashToken ext]

-- The type families below allow adding new parameters to each of the
-- base constructors of an expression which depend upon the phase
type family LamExtension phase
type family AppExtension phase
type family BindingExtension phase
type family ContentBindingExtension phase
type family CaseAnalysisExtension phase
type family SumExtension phase
type family ProductExtension phase
type family UnionExtension phase
type family BigLamExtension phase
type family BigAppExtension phase

-- The ExprExtension type family allows adding new constructors to the base Expr
-- type which depend upon the phase
type family ExprExtension phase

type family AbstractionFor phase
type family ContentBindingFor phase

-- LamF for phases where there is no extension to the constructor.
pattern Lam :: LamExtension phase ~ NoExt => AbstractionFor phase -> ExprFor phase -> ExprFor phase
pattern Lam abs expr <- FixPhase (LamF _ abs expr)
  where Lam abs expr =  FixPhase (LamF noExt abs expr)

pattern LamExt :: LamExtension phase -> AbstractionFor phase -> ExprFor phase -> ExprFor phase
pattern LamExt ext abs expr <- FixPhase (LamF ext abs expr)
  where LamExt ext abs expr =  FixPhase (LamF ext abs expr)

-- AppF for phases where there is no extension to the constructor.
pattern App :: AppExtension phase ~ NoExt => ExprFor phase -> ExprFor phase -> ExprFor phase
pattern App f x <- FixPhase (AppF _ f x)
  where App f x =  FixPhase (AppF noExt f x)

pattern AppExt :: AppExtension phase -> ExprFor phase -> ExprFor phase -> ExprFor phase
pattern AppExt ext f x <- FixPhase (AppF ext f x)
  where AppExt ext f x =  FixPhase (AppF ext f x)

-- BindingF for phases where there is no extension to the constructor.
pattern Binding :: BindingExtension phase ~ NoExt => BindingFor phase -> ExprFor phase
pattern Binding b <- FixPhase (BindingF _ b)
  where Binding b = FixPhase (BindingF noExt b)

pattern BindingExt :: BindingExtension phase -> BindingFor phase -> ExprFor phase
pattern BindingExt ext b <- FixPhase (BindingF ext b)
  where BindingExt ext b = FixPhase (BindingF ext b)

-- ContentBindingF for phases where there is no extension to the constructor.
pattern ContentBinding :: ContentBindingExtension phase ~ NoExt => ContentBindingFor phase -> ExprFor phase
pattern ContentBinding c <- FixPhase (ContentBindingF _ c)
  where ContentBinding c = FixPhase (ContentBindingF noExt c)

pattern ContentBindingExt :: ContentBindingExtension phase -> ContentBindingFor phase -> ExprFor phase
pattern ContentBindingExt ext c <- FixPhase (ContentBindingF ext c)
  where ContentBindingExt ext c = FixPhase (ContentBindingF ext c)

-- CaseAnalysisF for phases where there is no extension to the constructor.
pattern CaseAnalysis :: CaseAnalysisExtension phase ~ NoExt => Case (ExprFor phase) (PatternFor phase) -> ExprFor phase
pattern CaseAnalysis c <- FixPhase (CaseAnalysisF _ c)
  where CaseAnalysis c =  FixPhase (CaseAnalysisF noExt c)

pattern CaseAnalysisExt :: CaseAnalysisExtension phase -> Case (ExprFor phase) (PatternFor phase) -> ExprFor phase
pattern CaseAnalysisExt ext c <- FixPhase (CaseAnalysisF ext c)
  where CaseAnalysisExt ext c =  FixPhase (CaseAnalysisF ext c)

-- SumF for phases where there is no extension to the constructor.
pattern Sum :: SumExtension phase ~ NoExt =>  ExprFor phase -> Int -> NonEmpty (TypeFor phase) -> ExprFor phase
pattern Sum expr ix types <- FixPhase (SumF _ expr ix types)
  where Sum expr ix types =  FixPhase (SumF noExt expr ix types)

pattern SumExt :: SumExtension phase -> ExprFor phase -> Int -> NonEmpty (TypeFor phase) -> ExprFor phase
pattern SumExt ext expr ix types <- FixPhase (SumF ext expr ix types)
  where SumExt ext expr ix types =  FixPhase (SumF ext expr ix types)

-- ProductF for phases where there is no extension to the constructor.
pattern Product :: ProductExtension phase ~ NoExt =>  [ExprFor phase] -> ExprFor phase
pattern Product exprs <- FixPhase (ProductF _ exprs)
  where Product exprs =  FixPhase (ProductF noExt exprs)

-- The empty product for phases where there is no extension to the constructor.
pattern EmptyProduct :: ProductExtension phase ~ NoExt => ExprFor phase
pattern EmptyProduct <- FixPhase (ProductF _ [])
  where EmptyProduct =  FixPhase (ProductF noExt [])

pattern ProductExt :: ProductExtension phase ->  [ExprFor phase] -> ExprFor phase
pattern ProductExt ext exprs <- FixPhase (ProductF ext exprs)
  where ProductExt ext exprs =  FixPhase (ProductF ext exprs)

pattern EmptyProductExt :: ProductExtension phase -> ExprFor phase
pattern EmptyProductExt ext <- FixPhase (ProductF ext [])
  where EmptyProductExt ext =  FixPhase (ProductF ext [])

-- UnionF for phases where there is no extension to the constructor.
pattern Union :: UnionExtension phase ~ NoExt =>  ExprFor phase -> TypeFor phase -> Set.Set (TypeFor phase) -> ExprFor phase
pattern Union expr typeIx types <- FixPhase (UnionF _ expr typeIx types)
  where Union expr typeIx types =  FixPhase (UnionF noExt expr typeIx types)

pattern UnionExt :: UnionExtension phase ->  ExprFor phase -> TypeFor phase -> Set.Set (TypeFor phase) -> ExprFor phase
pattern UnionExt ext expr typeIx types <- FixPhase (UnionF ext expr typeIx types)
  where UnionExt ext expr typeIx types =  FixPhase (UnionF ext expr typeIx types)

-- BigLamF for phases where there is no extension to the constructor.
pattern BigLam :: BigLamExtension phase ~ NoExt =>  Kind -> ExprFor phase -> ExprFor phase
pattern BigLam typeAbs expr <- FixPhase (BigLamF _ typeAbs expr)
  where BigLam typeAbs expr =  FixPhase (BigLamF noExt typeAbs expr)

pattern BigLamExt :: BigLamExtension phase ->  Kind -> ExprFor phase -> ExprFor phase
pattern BigLamExt ext typeAbs expr <- FixPhase (BigLamF ext typeAbs expr)
  where BigLamExt ext typeAbs expr =  FixPhase (BigLamF ext typeAbs expr)

-- BigAppF for phases where there is no extension to the constructor.
pattern BigApp :: BigAppExtension phase ~ NoExt =>  ExprFor phase -> TypeFor phase -> ExprFor phase
pattern BigApp f xType <- FixPhase (BigAppF _ f xType)
  where BigApp f xType =  FixPhase (BigAppF noExt f xType)

pattern BigAppExt :: BigAppExtension phase ->  ExprFor phase -> TypeFor phase -> ExprFor phase
pattern BigAppExt ext f xType <- FixPhase (BigAppF ext f xType)
  where BigAppExt ext f xType =  FixPhase (BigAppF ext f xType)

-- ExprExtensionF for phases where there is no extension to the number of constructors.
pattern ExprExtension :: ExprExtension phase ~ NoExt =>  ExprFor phase
pattern ExprExtension <- FixPhase (ExprExtensionF _)
  where ExprExtension =  FixPhase (ExprExtensionF noExt)

pattern ExprExtensionExt :: ExprExtension phase ->  ExprFor phase
pattern ExprExtensionExt ext <- FixPhase (ExprExtensionF ext)
  where ExprExtensionExt ext =  FixPhase (ExprExtensionF ext)

-- In the Default phase:
-- - Bindings are Var's (indexes without names)
-- - Abstractions are Types (with no names)
-- - Type Bindings are TyVars (indexes without names)
-- - ContentBindings are extended with their checked Type
-- - There are no other extensions (such as comments on constructors)
-- This concrete phases can therefore be used for
-- {typechecking,reducing,printing,storing} but not {parsing,resolving}.
type instance LamExtension DefaultPhase = NoExt
type instance AppExtension DefaultPhase = NoExt
type instance BindingExtension DefaultPhase = NoExt
type instance ContentBindingExtension DefaultPhase = NoExt
type instance CaseAnalysisExtension DefaultPhase = NoExt
type instance SumExtension DefaultPhase = NoExt
type instance ProductExtension DefaultPhase = NoExt
type instance UnionExtension DefaultPhase = NoExt
type instance BigLamExtension DefaultPhase = NoExt
type instance BigAppExtension DefaultPhase = NoExt

-- TODO: Should _this_ be unit instead of noExt?
type instance ExprExtension DefaultPhase = NoExt

type instance ContentBindingFor DefaultPhase = ContentName
type instance AbstractionFor    DefaultPhase = Type

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

instance HasBinding Expr ContentName where
  applyToBinding f (ContentBindingExt typ c) = ContentBindingExt typ (f c)
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

type instance ErrorExpr DefaultPhase = Expr


-- | Under a binding context, type check an expression.
exprType
  :: TypeCheckCtx
  -> Expr
  -> Either Error Type
exprType ctx e =
   let withErrCtx = either (Left . EContext (ETypeChecking e)) Right
   in withErrCtx $ case e of

  -- ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     expr : exprTy
  -- ----------------------------------
  --   Lam absTy expr : absTy -> exprTy
  LamExt _ext absTy bodyExpr
    -> do let ctx' = case absTy of
                       TypeMuExt _ext _kind itselfType
                         -> ctx{_selfType = Just itselfType}
                       _ -> ctx
          exprTy <- exprType (underExpressionAbstraction absTy ctx') bodyExpr
          Right $ Arrow absTy exprTy

  --
  --   f : a -> b    x : a
  -- -----------------------
  --       App f x : b
  AppExt _ext f x
    -> do -- F must type check
          fTy <- exprType ctx f

          -- f can be a named type or content-binding as long as we can
          -- determine it's defined as an Arrow type.
          resolvedFTy <- case fTy of
            -- TODO: Looking up the initial type isnt necessarily necessary.
            -- We only need to prove the first level of the definition is an
            -- Arrow.
            NamedExt _ n
              -> case lookupTypeNameInitialType n (_typeCtx ctx) of
                   Nothing
                     -> Left . EContext (EMsg $ text "Reduction: function application") . ETypeNotDefined n . _typeCtx $ ctx

                   Just ty
                     -> Right ty

            TypeContentBindingExt _ c
              -> lookupTypeContentType c ctx

            -- There is no need to check for bindings as they will have been
            -- evaluated by exprType.
            fTy -> Right fTy

          case resolvedFTy of
            Arrow fromTy toTy
              -> do -- If the expected from-type is a Mu type, we construct the supplied x-type
                    (unconstructedXTy,constructedXTy) <- constructWhenTypeMu ctx fromTy x
                    case checkEqual fromTy constructedXTy ctx of
                          Left err
                            -> Left err

                          Right isSameType
                            | isSameType
                             -> Right toTy

                            | otherwise
                             -> Left $ EAppMismatch fTy unconstructedXTy
            _
              -> Left . EMsg . text $ "The first argument to an application did not have an arrow type"

  -- Provided an expression type checks and its type is in the correct place within a sum,
  -- has that sum type.
  SumExt _ext expr ix inTypr
    -> do -- Expression must type check
          exprTy <- exprType ctx expr

          -- Expression must have the type of the index in the sum it claims to have...
          sumTy <- if  NE.length inTypr <= ix
                     then Left $ EMsg $ text "Can't type check a sum because the index is larger than the number of types in the sum"
                     else Right (inTypr NE.!! ix)

          _ <- case checkEqual exprTy sumTy ctx of
                   Left err
                     -> Left . EContext (ESumMismatch exprTy ix inTypr) $ err
                     -- -> Left err

                   Right isSameType
                     | isSameType -> Right ()
                     | otherwise  -> Left $ ESumMismatch exprTy ix inTypr

          -- Allow the other types in the sum to not exist...
          _ <- Right ()

          -- Type is the claimed sum
          Right $ SumT inTypr

  -- A product is typed by the order of each expression it contains
  ProductExt _ext prodExprs
    -> do -- type check each successive expression
          prodExprTys <- mapM (exprType ctx) prodExprs

          -- the type is the product of those types
          Right $ ProductT prodExprTys

  -- Provided an expression typechecks and its type exists within the union, it has the claimed union type.
  -- TODO: Unused types in the union are not themselves checked for consistency
  -- TODO: The same type appearing more than once should be an error?
  UnionExt _ext unionExpr unionTypeIndex unionTypes
    -> do -- type check injected expression
          exprTy <- exprType ctx unionExpr

          -- Type must be what we claim it is...
          _ <- case checkEqual exprTy unionTypeIndex ctx of
                   Right True  -> Right ()
                   Right False -> Left $ EMsg $ text "Expression doesnt have the type within the union it claims to have"
                   Left err    -> Left err

          -- Type must be in the set somewhere...
          -- TODO
          _ <- if Set.member (Right True) . Set.map (\t -> checkEqual exprTy t ctx) $ unionTypes
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
  BindingExt _ext b
    -> lookupVarType b ctx

  -- At the type-checking phase we should have already resolved the name to
  -- ensure it's valid and extended the Content constructor with the contents
  -- type.
  --
  -- It is assumed the type has been type checked.
  --
  -- c : t
  -- -----
  -- c : t
  ContentBindingExt _ext c
    -> lookupExprContentType c ctx

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
  CaseAnalysisExt _ext c
    -> do -- The scrutinee expressions type:
          -- - Should be well-typed
          -- - Is reduced in the current current context
          -- - Is destructed a single level if it is a Mu type (the top-level
          --   SELFS are substituted with the type itself)
          scrutineeTy <- do initialTy <- exprType ctx (_caseScrutinee c)
                            reducedTy <- reduceTypeUnderCtx initialTy ctx
                            case reducedTy of
                              TypeMuExt _ _ itselfTy
                                -> do destructedTy <- destructTypeMuUnderCtx itselfTy ctx
                                      reduceTypeUnderCtx destructedTy ctx
                              ty
                                -> pure ty

          case _caseCaseBranches c of

            -- The case expression is then typed by the default branch, if its well typed
            DefaultOnly defExpr
              -> exprType ctx defExpr

            CaseBranches (branch0 :| branches) mDefExpr
              -> do -- Check the all the branches
                    branch0Ty <- branchType branch0 exprType scrutineeTy ctx
                    branchTys <- mapM (\branch -> branchType branch exprType scrutineeTy ctx) branches

                    -- Check the default branch if it exists
                    mDefExprTy <- maybe (Right Nothing) (fmap Just . exprType ctx) mDefExpr

                    -- If the default branch exists, its type must be the same as the first branch
                    _ <- maybe (Right ())
                               (\defExprTy -> case checkEqual defExprTy branch0Ty ctx of
                                                  Left err -> Left err
                                                  Right isSameType
                                                    | isSameType -> Right ()
                                                    | otherwise  -> Left $ ECaseDefaultMismatch defExprTy branch0Ty
                               )
                               mDefExprTy

                    -- Any other branches must have the same type as the first
                    _ <- mapM (\branchTy -> case checkEqual branchTy branch0Ty ctx of
                                                Left err -> Left err
                                                Right isSameType
                                                  | isSameType -> Right ()
                                                  | otherwise  -> Left . ECaseBranchMismatch branch0Ty $ branchTy
                              )
                              branchTys

                    -- The case expression has the type of all of the branch expressions
                    Right branch0Ty


  --    absKind :: kind      expr : exprTy
  -- ---------------------------------------
  --   BigLam absKind expr : kind BigArrow exprTy
  BigLamExt _ext absKind bodyExpr
    -> do exprTy <- exprType (underTypeAbstraction absKind ctx) bodyExpr
          Right $ BigArrow absKind exprTy

  --    f : aKind BigLamArrow bTy      xTy :: aKind
  -- ---------------------------------------------------
  --              BigApp f xTy : bTy
  BigAppExt _ext f x
    -> do -- The applied type must kind-check
          xKy <- kindCheck x ctx

          -- Check f under x
          fTy <- exprType (underAppliedType (x,xKy) ctx) f

          -- f can be a named type or content-binding as long as we can
          -- determine it's defined as a Big-Arrow type.
          resolvedFTy <- case fTy of
            -- TODO: Looking up the initial type isnt necessarily necessary.
            -- We only need to prove the first level of the definition is a
            -- Big-Arrow.
            NamedExt _ n
              -> case lookupTypeNameInitialType n (_typeCtx ctx) of
                   Nothing
                     -> Left . EContext (EMsg $ text "Reduction: Big application") . ETypeNotDefined n . _typeCtx $ ctx

                   Just ty
                     -> Right ty

            TypeContentBindingExt _ c
              -> lookupTypeContentType c ctx

            -- There is no need to check for bindings as they will have been
            -- evaluated by exprType.
            _
              -> Right fTy

          case resolvedFTy of
            BigArrow fromKy toTy
              | fromKy == xKy
               -> Right $ instantiate x toTy -- TODO: all bindings need to be instantiated

              | otherwise
               -> Left $ EBigAppMismatch fTy xKy

            _
              -> Left $ EMsg $ text "The first argument to a big application did not have a big arrow type"

  e -> error "Non-exhaustive pattern in type check"

-- | Gather the Set of all ContentBinding names used within an Expression
-- _without_ looking under any of the returned names themselves.
gatherContentNames
  :: forall phase
   . ContentBindingFor phase ~ ContentName
  => ExprFor phase
  -> Set ContentName
gatherContentNames = gatherContentNames' Set.empty
  where
    gatherContentNames' :: Set ContentName -> ExprFor phase -> Set ContentName
    gatherContentNames' accNames = \case
      ContentBindingExt _ext c
        -> Set.insert c accNames

      LamExt _ext abs expr
        -> gatherContentNames' accNames expr

      AppExt _ext f x
        -> gatherContentNames' (gatherContentNames' accNames x) f

      BindingExt _ext b
        -> Set.empty

      CaseAnalysisExt _ext c
        -> gatherCaseContentNames' accNames c

      SumExt _ext expr _ix _types
        -> gatherContentNames' accNames expr

      ProductExt _ext exprs
        -> foldr (flip gatherContentNames') accNames exprs

      UnionExt _ext expr _typeIx _types
        -> gatherContentNames' accNames expr

      BigLamExt _ext _kind expr
        -> gatherContentNames' accNames expr

      BigAppExt _ext fExpr _xType
        -> gatherContentNames' accNames fExpr

      _ -> error "Non-exhaustive pattern gathering names"

    gatherCaseContentNames' :: Set ContentName -> Case (ExprFor phase) (PatternFor phase) -> Set ContentName
    gatherCaseContentNames' accNames (Case expr branches) = gatherCaseBranchesContentNames' (gatherContentNames' accNames expr) branches

    gatherCaseBranchesContentNames' :: Set ContentName -> CaseBranches (ExprFor phase) (PatternFor phase) -> Set ContentName
    gatherCaseBranchesContentNames' accNames = \case
      DefaultOnly expr
        -> gatherContentNames' accNames expr

      CaseBranches neBranches mDef
        -> let accNames' = maybe accNames (gatherContentNames' accNames) mDef
            in foldr (flip gatherCaseBranchContentNames') accNames' neBranches

    gatherCaseBranchContentNames' :: Set ContentName -> CaseBranch (ExprFor phase) (PatternFor phase) -> Set ContentName
    gatherCaseBranchContentNames' accNames (CaseBranch _pattern exprResult) = gatherContentNames' accNames exprResult

-- | Gather the Set of all TypeContentBinding names used within an Expression
-- _without_ looking under any of the returned names themselves.
gatherExprsTypeContentNames
  :: forall phase
   . ( AbstractionFor phase ~ TypeFor phase
     , ContentName ~ TypeContentBindingFor phase
     )
  => ExprFor phase
  -> Set ContentName
gatherExprsTypeContentNames = gather Set.empty
  where
    gather :: Set ContentName -> ExprFor phase -> Set ContentName
    gather accNames = \case
      ContentBindingExt _ext c
        -> accNames

      LamExt _ext abs _expr
        -> Set.union accNames $ gatherTypeContentNames abs

      AppExt _ext f x
        -> gather (gather accNames x) f

      BindingExt _ext _b
        -> Set.empty

      CaseAnalysisExt _ext c
        -> gatherCase accNames c

      SumExt _ext expr _ix types
        -> gather (foldr Set.union accNames $ fmap gatherTypeContentNames types) expr

      ProductExt _ext exprs
        -> foldr (flip gather) accNames exprs

      UnionExt _ext expr _typeIx types
        -> gather (foldr Set.union accNames . fmap gatherTypeContentNames . Set.toList $ types) expr

      BigLamExt _ext _kind expr
        -> gather accNames expr

      BigAppExt _ext fExpr xType
        -> gather (accNames `Set.union` gatherTypeContentNames xType) fExpr

      _ -> error "Non-exhaustive pattern gathering names"

    gatherCase :: Set ContentName -> Case (ExprFor phase) (PatternFor phase) -> Set ContentName
    gatherCase accNames (Case expr branches) = gatherCaseBranches (gather accNames expr) branches

    gatherCaseBranches :: Set ContentName -> CaseBranches (ExprFor phase) (PatternFor phase) -> Set ContentName
    gatherCaseBranches accNames = \case
      DefaultOnly expr
        -> gather accNames expr

      CaseBranches neBranches mDef
        -> let accNames' = maybe accNames (gather accNames) mDef
            in foldr (flip gatherCaseBranch) accNames' neBranches

    gatherCaseBranch :: Set ContentName -> CaseBranch (ExprFor phase) (PatternFor phase) -> Set ContentName
    gatherCaseBranch accNames (CaseBranch _pattern exprResult) = gather accNames exprResult

-- Chain a list of expressions to be applied, left to right.
appise :: AppExtension phase ~ NoExt => [ExprFor phase] -> ExprFor phase
appise []        = error "Cant appise empty list of expressions"
appise [e]       = e
appise (e:e':es) = appise (App e e' : es)

-- Chain a list of abstractions, to be made left to right.
lamise :: (LamExtension phase ~ NoExt, AbstractionFor phase ~ TypeFor phase) => [TypeFor phase] -> ExprFor phase -> ExprFor phase
lamise []        _ = error "Cant lamise empty list of abstractions"
lamise [t]       e = Lam t e
lamise (t:t':ts) e = Lam t (lamise (t':ts) e)

-- Type check an expression, potentially under the context of a Mu self-type.
--
-- The first result is the original checked type. This will have been checked
-- under the self-type if present.
--
-- The second result is a potentially modified Mu type of the expression,
-- compatible with the provided Mu self-type.
constructWhenTypeMu
  :: TypeCheckCtx
  -> Type
  -> Expr
  -> Either Error (Type, Type)
constructWhenTypeMu ctx againstTy x = case againstTy of
  TypeMuExt _ext kind itselfTy
    -> do xTy <- exprType (ctx{_selfType = Just itselfTy}) x
          pure (xTy, TypeMuExt noExt kind xTy)

  _ -> do xTy <- exprType ctx x
          pure (xTy, xTy)

