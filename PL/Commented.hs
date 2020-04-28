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
  , CommentedExpr
  , CommentedType
  , CommentedPattern

  -- Expressions, types and matchargs are extended with the possibility for an
  -- associated comment
  , pattern CommentedExpr
  , pattern CommentedType
  , pattern CommentedPattern

  -- Convert a commented expression into an expression in the DefaultPhase
  -- with comments removed.
  , stripComments
  , stripTypeComments
  , stripCaseComments
  , stripCaseBranchesComments
  , stripCaseBranchComments
  , stripPatternComments

  -- Convert a DefaultPhase expression into one that can be decorated with
  -- comments
  , addComments
  , addTypeComments
  , addCaseComments
  , addCaseBranchesComments
  , addCaseBranchComments
  , addPatternComments
  )
  where

import PL.Expr
import PL.Type

import PL.Var
import PL.Case
import PL.TyVar
import PL.Name
import PL.Kind
import PL.FixPhase
import PL.Pattern

import Data.Text
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set

import Data.String

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
  deriving (Show, Eq, Ord)

instance IsString Comment where
  fromString str
    | elem '"' str = error $ "Cannot construct Comment from string as it contains a double quote character: " <> str
    | otherwise    = Comment . Text.pack $ str

-- | A thing is commented when it is associated with a comment.
data Commented e = Commented
  { _comment   :: Comment
  , _commentee :: e
  }
  deriving (Show, Eq, Ord)

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

-- No extensions to Pattern level constructors
type instance SumPatternExtension CommentedPhase = Void
type instance ProductPatternExtension CommentedPhase = Void
type instance UnionPatternExtension CommentedPhase = Void
type instance BindingPatternExtension CommentedPhase = Void
type instance BindExtension CommentedPhase = Void

-- | A Pattern is in the commented phase when it has an additional
-- constructor which may recursively contain Patterns wrapped in comments
type instance PatternExtension CommentedPhase = Commented (PatternFor CommentedPhase)

-- The commented phase uses variables and types for bindings and abstractions
type instance BindingFor     CommentedPhase = Var
type instance TypeBindingFor CommentedPhase = TyVar
type instance AbstractionFor CommentedPhase = TypeFor CommentedPhase

type CommentedExpr     = ExprFor     CommentedPhase
type CommentedType     = TypeFor     CommentedPhase
type CommentedPattern = PatternFor CommentedPhase

-- Pattern synonyms

-- TODO: Should these be more general to allow commenting currently un-commented
-- things?

-- New extension points wrap expressions, matchargs and types in comments.
pattern CommentedExpr :: Comment -> CommentedExpr -> CommentedExpr
pattern CommentedExpr comment expr <- FixPhase (ExprExtensionF (Commented comment expr))
  where CommentedExpr comment expr =  FixPhase (ExprExtensionF (Commented comment expr))

pattern CommentedPattern :: Comment -> CommentedPattern -> CommentedPattern
pattern CommentedPattern comment commentedPattern <- FixPhase (PatternExtensionF (Commented comment commentedPattern))
  where CommentedPattern comment commentedPattern =  FixPhase (PatternExtensionF (Commented comment commentedPattern))

pattern CommentedType :: Comment -> CommentedType -> CommentedType
pattern CommentedType comment commentedType <- FixPhase (TypeExtensionF (Commented comment commentedType))
  where CommentedType comment commentedType =  FixPhase (TypeExtensionF (Commented comment commentedType))

-- TODO: These functions could be written in terms of recursion schemes.

-- Strip comments from an expression
stripComments
  :: ExprFor CommentedPhase
  -> ExprFor DefaultPhase
stripComments = \case
  Lam ty expr
    -> Lam (stripTypeComments ty) (stripComments expr)

  App fExpr xExpr
    -> App (stripComments fExpr) (stripComments xExpr)

  CaseAnalysis c
     -> CaseAnalysis $ stripCaseComments c

  Sum expr ix ty
    -> Sum (stripComments expr) ix (fmap stripTypeComments ty)

  Product prodExprs
    -> Product (fmap stripComments prodExprs)

  Union unionExpr tyIx ty
    -> Union (stripComments unionExpr) (stripTypeComments tyIx) (Set.map stripTypeComments ty)

  Binding b
    -> Binding b

  BigLam takeTy expr
    -> BigLam takeTy (stripComments expr)

  BigApp fExpr xTy
    -> BigApp (stripComments fExpr) (stripTypeComments xTy)

  CommentedExpr _comment commentedExpr
    -> stripComments commentedExpr

  _ -> error "Non-exhaustive pattern in stripComments"

stripTypeComments
  :: TypeFor CommentedPhase
  -> TypeFor DefaultPhase
stripTypeComments = \case
  Named tyName
    -> Named tyName

  Arrow from to
    -> Arrow (stripTypeComments from) (stripTypeComments to)

  SumT types
    -> SumT (fmap stripTypeComments types)

  ProductT types
    -> ProductT (fmap stripTypeComments types)

  UnionT types
    -> UnionT (Set.map stripTypeComments $ types)

  BigArrow from to
    -> BigArrow from (stripTypeComments to)

  TypeLam kind typ
    -> TypeLam kind (stripTypeComments typ)

  TypeApp x y
    -> TypeApp (stripTypeComments x) (stripTypeComments y)

  TypeBinding b
    -> TypeBinding b

  CommentedType _comment commentedType
    -> stripTypeComments commentedType

  _ -> error "Non-exhaustive pattern in stripTypeComments"

stripCaseComments
  :: Case (ExprFor CommentedPhase) (PatternFor CommentedPhase)
  -> Case (ExprFor DefaultPhase)   (PatternFor DefaultPhase)
stripCaseComments (Case expr branches) = Case (stripComments expr) (stripCaseBranchesComments branches)

stripCaseBranchesComments
  :: CaseBranches (ExprFor CommentedPhase) (PatternFor CommentedPhase)
  -> CaseBranches (ExprFor DefaultPhase)   (PatternFor DefaultPhase)
stripCaseBranchesComments = \case
  DefaultOnly expr
    -> DefaultOnly (stripComments expr)

  CaseBranches neBranches mDefault
    -> CaseBranches (fmap stripCaseBranchComments neBranches) (fmap stripComments mDefault)

stripCaseBranchComments
  :: CaseBranch (ExprFor CommentedPhase) (PatternFor CommentedPhase)
  -> CaseBranch (ExprFor DefaultPhase)   (PatternFor DefaultPhase)
stripCaseBranchComments (CaseBranch match result) = CaseBranch (stripPatternComments match) (stripComments result)

stripPatternComments
  :: PatternFor CommentedPhase
  -> PatternFor DefaultPhase
stripPatternComments = \case
  Bind
    -> Bind

  BindingPattern b
    -> BindingPattern b

  SumPattern sumIndex nestedPattern
    -> SumPattern sumIndex (stripPatternComments nestedPattern)

  ProductPattern nestedPatterns
    -> ProductPattern (fmap stripPatternComments nestedPatterns)

  UnionPattern unionIndexTy nestedPattern
    -> UnionPattern (stripTypeComments unionIndexTy) (stripPatternComments nestedPattern)

  CommentedPattern _comment commentedPattern
    -> stripPatternComments commentedPattern

  _ -> error "Non-exhaustive pattern match when stripping comments from match statement"

addComments
  :: ExprFor DefaultPhase
  -> ExprFor CommentedPhase
addComments = \case
  Lam ty expr
    -> Lam (addTypeComments ty) (addComments expr)

  App fExpr xExpr
    -> App (addComments fExpr) (addComments xExpr)

  CaseAnalysis c
     -> CaseAnalysis $ addCaseComments c

  Sum expr ix ty
    -> Sum (addComments expr) ix (fmap addTypeComments ty)

  Product prodExprs
    -> Product (fmap addComments prodExprs)

  Union unionExpr tyIx ty
    -> Union (addComments unionExpr) (addTypeComments tyIx) (Set.map addTypeComments ty)

  Binding b
    -> Binding b

  BigLam takeTy expr
    -> BigLam takeTy (addComments expr)

  BigApp fExpr xTy
    -> BigApp (addComments fExpr) (addTypeComments xTy)

  _ -> error "Non-exhaustive pattern adding comments"

addTypeComments
  :: TypeFor DefaultPhase
  -> TypeFor CommentedPhase
addTypeComments = \case
  Named tyName
    -> Named tyName

  Arrow from to
    -> Arrow (addTypeComments from) (addTypeComments to)

  SumT types
    -> SumT (fmap addTypeComments types)

  ProductT types
    -> ProductT (fmap addTypeComments types)

  UnionT types
    -> UnionT (Set.map addTypeComments $ types)

  BigArrow from to
    -> BigArrow from (addTypeComments to)

  TypeLam kind typ
    -> TypeLam kind (addTypeComments typ)

  TypeApp x y
    -> TypeApp (addTypeComments x) (addTypeComments y)

  TypeBinding b
    -> TypeBinding b

  _ -> error "Non-exhaustive pattern in addTypeComments"

addCaseComments
  :: Case (ExprFor DefaultPhase) (PatternFor DefaultPhase)
  -> Case (ExprFor CommentedPhase) (PatternFor CommentedPhase)
addCaseComments (Case expr branches) = Case (addComments expr) (addCaseBranchesComments branches)

addCaseBranchesComments
  :: CaseBranches (ExprFor DefaultPhase) (PatternFor DefaultPhase)
  -> CaseBranches (ExprFor CommentedPhase) (PatternFor CommentedPhase)
addCaseBranchesComments = \case
  DefaultOnly expr
    -> DefaultOnly (addComments expr)

  CaseBranches neBranches mDefault
    -> CaseBranches (fmap addCaseBranchComments neBranches) (fmap addComments mDefault)

addCaseBranchComments
  :: CaseBranch (ExprFor DefaultPhase) (PatternFor DefaultPhase)
  -> CaseBranch (ExprFor CommentedPhase) (PatternFor CommentedPhase)
addCaseBranchComments (CaseBranch match result) = CaseBranch (addPatternComments match) (addComments result)

addPatternComments
  :: PatternFor DefaultPhase
  -> PatternFor CommentedPhase
addPatternComments = \case
  Bind
    -> Bind

  BindingPattern b
    -> BindingPattern b

  SumPattern sumIndex nestedPattern
    -> SumPattern sumIndex (addPatternComments nestedPattern)

  ProductPattern nestedPatterns
    -> ProductPattern (fmap addPatternComments nestedPatterns)

  UnionPattern unionIndexTy nestedPattern
    -> UnionPattern (addTypeComments unionIndexTy) (addPatternComments nestedPattern)

  _ -> error "Non-exhaustive pattern match when add comments from match statement"

-- stripComments commentedExpr == strippedExpr
commentedExpr :: ExprFor CommentedPhase
commentedExpr = CommentedExpr (Comment "Ignore value typed Foo and return value typed Bar") $ Lam (Named "Foo") $ Lam (Named "Bar") $ Binding VZ

strippedExpr :: ExprFor DefaultPhase
strippedExpr = Lam (Named "Foo") $ Lam (Named "Bar") $ Binding VZ

