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
Module      : PL.Commented
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Comments associate a string of text with a thing.

This module exports a CommentedPhase which associates comments with expressions,
types and matchargs by using an extension constructor (rather than adding a new
comment field to each alternative constructor).

Patterns are provided for constructing and matching on commented things.
-}

module PL.Commented
  ( -- Core comment structure
    CommentedPhase
  , Comment (..)
  , Commented (..)

  -- Aliases in the CommentedPhase
  , Expr
  , Type
  , MatchArg

  -- Expressions, types and matchargs are extended with the possibility for an
  -- associated comment
  , pattern CommentedExpr
  , pattern CommentedType
  , pattern CommentedMatchArg

  , pattern Lam
  , pattern App
  , pattern Binding
  , pattern CaseAnalysis
  , pattern Sum
  , pattern Product
  , pattern Union
  , pattern BigLam
  , pattern BigApp

  , pattern MatchSum
  , pattern MatchProduct
  , pattern MatchUnion
  , pattern MatchBinding
  , pattern Bind

  , pattern Named
  , pattern Arrow
  , pattern SumT
  , pattern ProductT
  , pattern UnionT
  , pattern BigArrow
  , pattern TypeLam
  , pattern TypeApp
  , pattern TypeBinding

  -- Convert a commented expression into an expression in the DefaultPhase
  -- with comments removed.
  , stripComments
  , stripTypeComments
  , stripCaseComments
  , stripCaseBranchesComments
  , stripCaseBranchComments
  , stripMatchArgComments
  )
  where

-- Import only the extension points for expressions
-- leaving patterns to be qualified by the DefaultPhase
import PL.Expr
  ( ExprF(..)
  , ExprFor
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

  , MatchArgF(..)
  , MatchArgFor
  , MatchSumExtension
  , MatchProductExtension
  , MatchUnionExtension
  , MatchBindingExtension
  , BindExtension
  , MatchArgExtension
  )
import qualified PL.Expr as DefaultPhase

-- Import only the extension points for types
-- leaving patterns to be qualified by the DefaultPhase
import PL.Type
  ( TypeF(..)
  , NamedExtension
  , ArrowExtension
  , SumTExtension
  , ProductTExtension
  , UnionTExtension
  , BigArrowExtension
  , TypeLamExtension
  , TypeAppExtension
  , TypeBindingExtension
  , TypeExtension

  , TypeBindingFor

  , TypeFor

  -- Dont belong there:
  , DefaultPhase
  , void
  , Void
  )
import qualified PL.Type as DefaultPhase

import PL.Var
import PL.Case
import PL.TyVar
import PL.Name
import PL.Kind
import PL.FixPhase

import Data.Text
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set

-- | The phase where an expression is no more than a structurally sound AST with
-- optional comments around nodes.
--
-- Expressions in this phase are output by parsers.
-- They may be consumed by typecheckers or printers.
data CommentedPhase

-- | A comment is unstructured text.
data Comment = Comment
  { _commentText :: Text
  }
  deriving Show

-- | A thing is commented when it is associated with a comment.
data Commented e = Commented
  { _comment   :: Comment
  , _commentee :: e
  }
  deriving Show

-- | A Type is in the commented phase when is has an additional constructor
-- which may recursively contain types wrapped in comments.
type instance TypeExtension CommentedPhase = Commented (TypeFor CommentedPhase)

-- No extensions to Type level constructors
-- An alternative would be to store comments on each constructor.
-- We've currently chosen wrapping instead.
type instance NamedExtension CommentedPhase = Void
type instance ArrowExtension CommentedPhase = Void
type instance SumTExtension CommentedPhase = Void
type instance ProductTExtension CommentedPhase = Void
type instance UnionTExtension CommentedPhase = Void
type instance BigArrowExtension CommentedPhase = Void
type instance TypeLamExtension CommentedPhase = Void
type instance TypeAppExtension CommentedPhase = Void
type instance TypeBindingExtension CommentedPhase = Void

-- No extensions to Expr level constructors
type instance LamExtension CommentedPhase = Void
type instance AppExtension CommentedPhase = Void
type instance BindingExtension CommentedPhase = Void
type instance CaseAnalysisExtension CommentedPhase = Void
type instance SumExtension CommentedPhase = Void
type instance ProductExtension CommentedPhase = Void
type instance UnionExtension CommentedPhase = Void
type instance BigLamExtension CommentedPhase = Void
type instance BigAppExtension CommentedPhase = Void

-- | An expression is in the commented phase when it has an additional
-- constructor which may recursively contain expressions wrapped in comments
type instance ExprExtension CommentedPhase = Commented (ExprFor CommentedPhase)

-- No extensions to MatchArg level constructors
type instance MatchSumExtension CommentedPhase = Void
type instance MatchProductExtension CommentedPhase = Void
type instance MatchUnionExtension CommentedPhase = Void
type instance MatchBindingExtension CommentedPhase = Void
type instance BindExtension CommentedPhase = Void

-- | A MatchArg is in the commented phase when it has an additional
-- constructor which may recursively contain MatchArgs wrapped in comments
type instance MatchArgExtension CommentedPhase = Commented (MatchArgFor CommentedPhase)

-- The commented phase uses variables and types for bindings and abstractions
type instance BindingFor     CommentedPhase = Var
type instance TypeBindingFor CommentedPhase = TyVar
type instance AbstractionFor CommentedPhase = TypeFor CommentedPhase

type Expr     = ExprFor     CommentedPhase
type Type     = TypeFor     CommentedPhase
type MatchArg = MatchArgFor CommentedPhase

-- Pattern synonyms

-- New extension point wraps expressions in comments
pattern CommentedExpr :: Comment -> Expr -> Expr
pattern CommentedExpr comment expr <- FixPhase (ExprExtensionF (Commented comment expr))
  where CommentedExpr comment expr =  FixPhase (ExprExtensionF (Commented comment expr))

pattern Lam :: Type -> Expr -> Expr
pattern Lam abs expr <- FixPhase (LamF _ abs expr)
  where Lam abs expr =  FixPhase (LamF void abs expr)

pattern App :: Expr -> Expr -> Expr
pattern App f x <- FixPhase (AppF _ f x)
  where App f x =  FixPhase (AppF void f x)

pattern Binding :: Var -> Expr
pattern Binding b <- FixPhase (BindingF _ b)
  where Binding b = FixPhase (BindingF void b)

pattern CaseAnalysis :: Case Expr MatchArg -> Expr
pattern CaseAnalysis c <- FixPhase (CaseAnalysisF _ c)
  where CaseAnalysis c =  FixPhase (CaseAnalysisF void c)

pattern Sum :: Expr -> Int -> NonEmpty Type -> Expr
pattern Sum expr ix types <- FixPhase (SumF _ expr ix types)
  where Sum expr ix types =  FixPhase (SumF void expr ix types)

pattern Product :: [Expr] -> Expr
pattern Product exprs <- FixPhase (ProductF _ exprs)
  where Product exprs =  FixPhase (ProductF void exprs)

pattern Union :: Expr -> Type -> Set.Set Type -> Expr
pattern Union expr typeIx types <- FixPhase (UnionF _ expr typeIx types)
  where Union expr typeIx types =  FixPhase (UnionF void expr typeIx types)

pattern BigLam :: Kind -> Expr -> Expr
pattern BigLam typeAbs expr <- FixPhase (BigLamF _ typeAbs expr)
  where BigLam typeAbs expr =  FixPhase (BigLamF void typeAbs expr)

pattern BigApp :: Expr -> Type -> Expr
pattern BigApp f xType <- FixPhase (BigAppF _ f xType)
  where BigApp f xType =  FixPhase (BigAppF void f xType)

-- New extension point wraps matchargs in comments
pattern CommentedMatchArg :: Comment -> MatchArg -> MatchArg
pattern CommentedMatchArg comment commentedMatchArg <- FixPhase (MatchArgExtensionF (Commented comment commentedMatchArg))
  where CommentedMatchArg comment commentedMatchArg =  FixPhase (MatchArgExtensionF (Commented comment commentedMatchArg))

pattern MatchSum :: Int -> MatchArg -> MatchArg
pattern MatchSum ix match <- FixPhase (MatchSumF _ ix match)
  where MatchSum ix match =  FixPhase (MatchSumF void ix match)

pattern MatchProduct :: [MatchArg] -> MatchArg
pattern MatchProduct matches <- FixPhase (MatchProductF _ matches)
  where MatchProduct matches =  FixPhase (MatchProductF void matches)

pattern MatchUnion :: Type -> MatchArg -> MatchArg
pattern MatchUnion typeIx match <- FixPhase (MatchUnionF _ typeIx match)
  where MatchUnion typeIx match =  FixPhase (MatchUnionF void typeIx match)

pattern MatchBinding :: Var -> MatchArg
pattern MatchBinding equalTo <- FixPhase (MatchBindingF _ equalTo)
  where MatchBinding equalTo =  FixPhase (MatchBindingF void equalTo)

pattern Bind :: MatchArg
pattern Bind <- FixPhase (BindF _)
  where Bind =  FixPhase (BindF void)

-- New extension point wraps types in comments
pattern CommentedType :: Comment -> Type -> Type
pattern CommentedType comment commentedType <- FixPhase (TypeExtensionF (Commented comment commentedType))
  where CommentedType comment commentedType =  FixPhase (TypeExtensionF (Commented comment commentedType))

pattern Named :: TypeName -> Type
pattern Named name <- FixPhase (NamedF _ name)
  where Named name =  FixPhase (NamedF void name)

pattern Arrow :: Type -> Type -> Type
pattern Arrow fromTy toTy <- FixPhase (ArrowF _ fromTy toTy)
  where Arrow fromTy toTy =  FixPhase (ArrowF void fromTy toTy)

pattern SumT :: NonEmpty Type -> Type
pattern SumT types <- FixPhase (SumTF _ types)
  where SumT types =  FixPhase (SumTF void types)

pattern ProductT :: [Type] -> Type
pattern ProductT types <- FixPhase (ProductTF _ types)
  where ProductT types =  FixPhase (ProductTF void types)

pattern UnionT :: Set.Set Type -> Type
pattern UnionT types <- FixPhase (UnionTF _ types)
  where UnionT types =  FixPhase (UnionTF void types)

pattern BigArrow :: Kind -> Type -> Type
pattern BigArrow kind ty <- FixPhase (BigArrowF _ kind ty)
  where BigArrow kind ty =  FixPhase (BigArrowF void kind ty)

pattern TypeLam :: Kind -> Type -> Type
pattern TypeLam absTy ty <- FixPhase (TypeLamF _ absTy ty)
  where TypeLam absTy ty =  FixPhase (TypeLamF void absTy ty)

pattern TypeApp :: Type -> Type -> Type
pattern TypeApp fTy xTy <- FixPhase (TypeAppF _ fTy xTy)
  where TypeApp fTy xTy =  FixPhase (TypeAppF void fTy xTy)

pattern TypeBinding :: TyVar -> Type
pattern TypeBinding tyVar <- FixPhase (TypeBindingF _ tyVar)
  where TypeBinding tyVar =  FixPhase (TypeBindingF void tyVar)

-- TODO: These functions could be written in terms of recursion schemes.

-- Strip comments from an expression
stripComments
  :: ExprFor CommentedPhase
  -> ExprFor DefaultPhase
stripComments = \case
  Lam ty expr
    -> DefaultPhase.Lam (stripTypeComments ty) (stripComments expr)

  App fExpr xExpr
    -> DefaultPhase.App (stripComments fExpr) (stripComments xExpr)

  CaseAnalysis c
     -> DefaultPhase.CaseAnalysis $ stripCaseComments c

  Sum expr ix ty
    -> DefaultPhase.Sum (stripComments expr) ix (fmap stripTypeComments ty)

  Product prodExprs
    -> DefaultPhase.Product (fmap stripComments prodExprs)

  Union unionExpr tyIx ty
    -> DefaultPhase.Union (stripComments unionExpr) (stripTypeComments tyIx) (Set.map stripTypeComments ty)

  Binding b
    -> DefaultPhase.Binding b

  BigLam takeTy expr
    -> DefaultPhase.BigLam takeTy (stripComments expr)

  BigApp fExpr xTy
    -> DefaultPhase.BigApp (stripComments fExpr) (stripTypeComments xTy)

  CommentedExpr _comment commentedExpr
    -> stripComments commentedExpr

  _ -> error "Non-exhaustive pattern in stripComments"

stripTypeComments
  :: TypeFor CommentedPhase
  -> TypeFor DefaultPhase
stripTypeComments = \case
  Named tyName
    -> DefaultPhase.Named tyName

  Arrow from to
    -> DefaultPhase.Arrow (stripTypeComments from) (stripTypeComments to)

  SumT types
    -> DefaultPhase.SumT (fmap stripTypeComments types)

  ProductT types
    -> DefaultPhase.ProductT (fmap stripTypeComments types)

  UnionT types
    -> DefaultPhase.UnionT (Set.map stripTypeComments $ types)

  BigArrow from to
    -> DefaultPhase.BigArrow from (stripTypeComments to)

  TypeLam kind typ
    -> DefaultPhase.TypeLam kind (stripTypeComments typ)

  TypeApp x y
    -> DefaultPhase.TypeApp (stripTypeComments x) (stripTypeComments y)

  TypeBinding b
    -> DefaultPhase.TypeBinding b

  CommentedType _comment commentedType
    -> stripTypeComments commentedType

  _ -> error "Non-exhaustive pattern in stripTypeComments"

stripCaseComments
  :: Case (ExprFor CommentedPhase) (MatchArgFor CommentedPhase)
  -> Case (ExprFor DefaultPhase)   (MatchArgFor DefaultPhase)
stripCaseComments (Case expr branches) = Case (stripComments expr) (stripCaseBranchesComments branches)

stripCaseBranchesComments
  :: CaseBranches (ExprFor CommentedPhase) (MatchArgFor CommentedPhase)
  -> CaseBranches (ExprFor DefaultPhase)   (MatchArgFor DefaultPhase)
stripCaseBranchesComments = \case
  DefaultOnly expr
    -> DefaultOnly (stripComments expr)

  CaseBranches neBranches mDefault
    -> CaseBranches (fmap stripCaseBranchComments neBranches) (fmap stripComments mDefault)

stripCaseBranchComments
  :: CaseBranch (ExprFor CommentedPhase) (MatchArgFor CommentedPhase)
  -> CaseBranch (ExprFor DefaultPhase)   (MatchArgFor DefaultPhase)
stripCaseBranchComments (CaseBranch match result) = CaseBranch (stripMatchArgComments match) (stripComments result)

stripMatchArgComments
  :: MatchArgFor CommentedPhase
  -> MatchArgFor DefaultPhase
stripMatchArgComments = \case
  Bind
    -> DefaultPhase.Bind

  MatchBinding b
    -> DefaultPhase.MatchBinding b

  MatchSum sumIndex nestedMatchArg
    -> DefaultPhase.MatchSum sumIndex (stripMatchArgComments nestedMatchArg)

  MatchProduct nestedMatchArgs
    -> DefaultPhase.MatchProduct (fmap stripMatchArgComments nestedMatchArgs)

  MatchUnion unionIndexTy nestedMatchArg
    -> DefaultPhase.MatchUnion (stripTypeComments unionIndexTy) (stripMatchArgComments nestedMatchArg)

  CommentedMatchArg _comment commentedMatchArg
    -> stripMatchArgComments commentedMatchArg

  _ -> error "Non-exhaustive pattern match when stripping comments from match statement"


-- stripComments commentedExpr == strippedExpr
commentedExpr :: ExprFor CommentedPhase
commentedExpr = CommentedExpr (Comment "Ignore value typed Foo and return value typed Bar") $ Lam (Named "Foo") $ Lam (Named "Bar") $ Binding VZ

strippedExpr :: ExprFor DefaultPhase
strippedExpr = DefaultPhase.Lam (DefaultPhase.Named "Foo") $ DefaultPhase.Lam (DefaultPhase.Named "Bar") $ DefaultPhase.Binding VZ

