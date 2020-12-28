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
   , PolyKinds
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
    Comment (..)
  , Commented (..)

  -- Expressions, types and patterns are extended with the possibility for an
  -- associated comment
  , pattern CommentedExpr
  , pattern CommentedType
  , pattern CommentedPattern

    -- A concrete phase where expr, types and patterns are commented but all
    -- other extensions are NoExt.
  , CommentedPhase

  -- Aliases which extend phases with comments
  , CommentedExprFor
  , CommentedTypeFor
  , CommentedPatternFor

  -- Establish a has-comments and with-comments-removed relation between two
  -- phases which are otherwise identical
  , ExprWithoutComments
  , TypeWithoutComments
  , PatternWithoutComments

  -- Reverse *WithoutComments
  , ExprWithComments
  , TypeWithComments
  , PatternWithComments

  -- Remove the comment extension from phases, otherwise keeping them
  -- the same.
  , stripComments
  , stripTypeComments
  , stripCaseComments
  , stripCaseBranchesComments
  , stripCaseBranchComments
  , stripPatternComments

  -- Add the comment extension to phases, otherwise keeping them the same.
  , addComments
  , addTypeComments
  , addCaseComments
  , addCaseBranchesComments
  , addCaseBranchComments
  , addPatternComments
  )
  where

-- PL
import PL.Case
import PL.Expr
import PL.FixPhase
import PL.Kind
import PL.Name
import PL.Pattern
import PL.TyVar
import PL.Type
import PL.Var

-- External PL
import PLHash
import PLHash.Short

-- Other
import Data.List.NonEmpty (NonEmpty (..))
import Data.String
import Data.Text
import qualified Data.Set as Set
import qualified Data.Text as Text

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

-- Comments do not contribute to a Hash
instance Hashable e => Hashable (Commented e) where
  toHashToken (Commented c e) = toHashToken e

-- | A Type is in the commented phase when is has an additional constructor
-- which may recursively contain types wrapped in comments.
type instance TypeExtension CommentedPhase = Commented (TypeFor CommentedPhase)

-- No extensions to Type level constructors
-- An alternative would be to store comments on each constructor.
-- We've currently chosen wrapping instead.
type instance NamedExtension CommentedPhase = NoExt
type instance ArrowExtension CommentedPhase = NoExt
type instance SumTExtension CommentedPhase = NoExt
type instance ProductTExtension CommentedPhase = NoExt
type instance UnionTExtension CommentedPhase = NoExt
type instance BigArrowExtension CommentedPhase = NoExt
type instance TypeLamExtension CommentedPhase = NoExt
type instance TypeAppExtension CommentedPhase = NoExt
type instance TypeBindingExtension CommentedPhase = NoExt
type instance TypeContentBindingExtension CommentedPhase = NoExt

-- No extensions to Expr level constructors
type instance LamExtension CommentedPhase = NoExt
type instance AppExtension CommentedPhase = NoExt
type instance BindingExtension CommentedPhase = NoExt
type instance ContentBindingExtension CommentedPhase = NoExt
type instance CaseAnalysisExtension CommentedPhase = NoExt
type instance SumExtension CommentedPhase = NoExt
type instance ProductExtension CommentedPhase = NoExt
type instance UnionExtension CommentedPhase = NoExt
type instance BigLamExtension CommentedPhase = NoExt
type instance BigAppExtension CommentedPhase = NoExt

-- | An expression is in the commented phase when it has an additional
-- constructor which may recursively contain expressions wrapped in comments
type instance ExprExtension CommentedPhase = Commented (ExprFor CommentedPhase)

-- No extensions to Pattern level constructors
type instance SumPatternExtension CommentedPhase = NoExt
type instance ProductPatternExtension CommentedPhase = NoExt
type instance UnionPatternExtension CommentedPhase = NoExt
type instance BindingPatternExtension CommentedPhase = NoExt
type instance BindExtension CommentedPhase = NoExt

-- | A Pattern is in the commented phase when it has an additional
-- constructor which may recursively contain Patterns wrapped in comments
type instance PatternExtension CommentedPhase = Commented (PatternFor CommentedPhase)

-- The commented phase uses variables and types for bindings and abstractions
type instance BindingFor            CommentedPhase = Var
type instance ContentBindingFor     CommentedPhase = ShortHash
type instance TypeBindingFor        CommentedPhase = TyVar
type instance TypeContentBindingFor CommentedPhase = ShortHash
type instance AbstractionFor        CommentedPhase = TypeFor CommentedPhase

-- | Expressions at some phase that have been extended with a commented
-- expression constructor.
type CommentedExprFor phase = ExprExtension phase ~ (Commented (ExprFor phase))

-- | Types at some phase that have been extended with a commented type
-- constructor.
type CommentedTypeFor    phase = TypeExtension phase    ~ (Commented (TypeFor phase))

-- | Patterns at some phase that have been extended with a commented pattern
-- constructor.
type CommentedPatternFor phase = PatternExtension phase ~ (Commented (PatternFor phase))

-- Pattern synonyms

-- New extension points wrap expressions, matchargs and types in comments.
pattern CommentedExpr :: CommentedExprFor phase => Comment -> ExprFor phase -> ExprFor phase
pattern CommentedExpr comment expr <- FixPhase (ExprExtensionF (Commented comment expr))
  where CommentedExpr comment expr =  FixPhase (ExprExtensionF (Commented comment expr))

pattern CommentedPattern :: CommentedPatternFor phase => Comment -> PatternFor phase -> PatternFor phase
pattern CommentedPattern comment commentedPattern <- FixPhase (PatternExtensionF (Commented comment commentedPattern))
  where CommentedPattern comment commentedPattern =  FixPhase (PatternExtensionF (Commented comment commentedPattern))

pattern CommentedType :: CommentedTypeFor phase => Comment -> TypeFor phase -> TypeFor phase
pattern CommentedType comment commentedType <- FixPhase (TypeExtensionF (Commented comment commentedType))
  where CommentedType comment commentedType =  FixPhase (TypeExtensionF (Commented comment commentedType))

-- | Two expression phases are identical but the first contains comments and the
-- second is free to vary.
type ExprWithoutComments phaseIn phaseOut =
     ( -- The first expression is commented
       CommentedExprFor phaseIn

     , PatternWithoutComments phaseIn phaseOut
     , TypeWithoutComments    phaseIn phaseOut

       -- Both expressions use types for abstractions
     , AbstractionFor phaseIn ~ TypeFor phaseIn
     , AbstractionFor phaseOut ~ TypeFor phaseOut

     , Eq (Commented (TypeFor phaseIn))
     , Ord (NamedExtension phaseIn)

       -- Otherwise, both expressions have the same extensions
     , LamExtension            phaseIn ~ LamExtension            phaseOut
     , AppExtension            phaseIn ~ AppExtension            phaseOut
     , BindingExtension        phaseIn ~ BindingExtension        phaseOut
     , ContentBindingExtension phaseIn ~ ContentBindingExtension phaseOut
     , CaseAnalysisExtension   phaseIn ~ CaseAnalysisExtension   phaseOut
     , SumExtension            phaseIn ~ SumExtension            phaseOut
     , ProductExtension        phaseIn ~ ProductExtension        phaseOut
     , UnionExtension          phaseIn ~ UnionExtension          phaseOut
     , BigLamExtension         phaseIn ~ BigLamExtension         phaseOut
     , BigAppExtension         phaseIn ~ BigAppExtension         phaseOut

       -- Both expressions use the same binding and abstraction types.
     , BindingFor        phaseIn ~ BindingFor        phaseOut
     , ContentBindingFor phaseIn ~ ContentBindingFor phaseOut
     , TypeBindingFor    phaseIn ~ TypeBindingFor    phaseOut
     )

-- | Two expression phases are identical except the second contains comments and
-- the first is free to vary.
type ExprWithComments phaseIn phaseOut = ExprWithoutComments phaseOut phaseIn

-- | Two type phases are identical but the first contains comments and the
-- second is free to vary.
type TypeWithoutComments phaseIn phaseOut =
     ( -- The first type is commented
       CommentedTypeFor phaseIn

     , Ord (TypeFor phaseIn)
     , Ord (TypeFor phaseOut)
     , Eq  (TypeFor phaseIn)
     , Eq  (TypeFor phaseOut)

       -- Otherwise, both types have the same extensions
     , NamedExtension              phaseIn ~ NamedExtension              phaseOut
     , ArrowExtension              phaseIn ~ ArrowExtension              phaseOut
     , SumTExtension               phaseIn ~ SumTExtension               phaseOut
     , ProductTExtension           phaseIn ~ ProductTExtension           phaseOut
     , UnionTExtension             phaseIn ~ UnionTExtension             phaseOut
     , BigArrowExtension           phaseIn ~ BigArrowExtension           phaseOut
     , TypeLamExtension            phaseIn ~ TypeLamExtension            phaseOut
     , TypeAppExtension            phaseIn ~ TypeAppExtension            phaseOut
     , TypeBindingExtension        phaseIn ~ TypeBindingExtension        phaseOut
     , TypeContentBindingExtension phaseIn ~ TypeContentBindingExtension phaseOut

     , TypeBindingFor phaseIn ~ TypeBindingFor phaseOut
     , TypeContentBindingFor phaseIn ~ TypeContentBindingFor phaseOut
     )

-- | Two type phases are identical except the second contains comments and
-- the first is free to vary.
type TypeWithComments phaseIn phaseOut = TypeWithoutComments phaseOut phaseIn

-- | Two pattern phases are identical but the first contains comments and the
-- second is free to vary.
type PatternWithoutComments phaseIn phaseOut =
     ( -- The first pattern is commented
       CommentedPatternFor phaseIn

     , TypeWithoutComments phaseIn phaseOut

      -- Otherwise, both patterns have the same extensions
     , SumPatternExtension     phaseIn ~ SumPatternExtension     phaseOut
     , ProductPatternExtension phaseIn ~ ProductPatternExtension phaseOut
     , UnionPatternExtension   phaseIn ~ UnionPatternExtension   phaseOut
     , BindingPatternExtension phaseIn ~ BindingPatternExtension phaseOut
     , BindExtension           phaseIn ~ BindExtension           phaseOut

     , BindingFor phaseIn ~ BindingFor phaseOut
     )

-- | Two pattern phases are identical except the second contains comments and
-- the first is free to vary.
type PatternWithComments phaseIn phaseOut = PatternWithoutComments phaseOut phaseIn

-- TODO: These functions could be written in terms of recursion schemes.

-- | Strip comments from an expression.
stripComments
  :: ExprWithoutComments phaseIn phaseOut
  => ExprFor phaseIn
  -> ExprFor phaseOut
stripComments e = case e of
  FixPhase (ExprExtensionF (Commented _comment commentedExpr))
    -> stripComments commentedExpr

  LamExt ext ty expr
    -> LamExt ext (stripTypeComments ty) (stripComments expr)

  AppExt ext fExpr xExpr
    -> AppExt ext (stripComments fExpr) (stripComments xExpr)

  CaseAnalysisExt ext c
     -> CaseAnalysisExt ext $ stripCaseComments c

  SumExt ext expr ix ty
    -> SumExt ext (stripComments expr) ix (fmap stripTypeComments ty)

  ProductExt ext prodExprs
    -> ProductExt ext (fmap stripComments prodExprs)

  UnionExt ext unionExpr tyIx ty
    -> UnionExt ext (stripComments unionExpr) (stripTypeComments tyIx) (Set.map stripTypeComments ty)

  BindingExt ext b
    -> BindingExt ext b

  ContentBindingExt ext c
    -> ContentBindingExt ext c

  BigLamExt ext takeTy expr
    -> BigLamExt ext takeTy (stripComments expr)

  BigAppExt ext fExpr xTy
    -> BigAppExt ext (stripComments fExpr) (stripTypeComments xTy)

  _ -> error "Non-exhaustive pattern in stripComments"

-- | Strip comments from a type.
stripTypeComments
  :: TypeWithoutComments phaseIn phaseOut
  => TypeFor phaseIn
  -> TypeFor phaseOut
stripTypeComments = \case
  FixPhase (TypeExtensionF (Commented _comment commentedType))
    -> stripTypeComments commentedType

  NamedExt ext tyName
    -> NamedExt ext tyName

  ArrowExt ext from to
    -> ArrowExt ext (stripTypeComments from) (stripTypeComments to)

  SumTExt ext types
    -> SumTExt ext (fmap stripTypeComments types)

  ProductTExt ext types
    -> ProductTExt ext (fmap stripTypeComments types)

  UnionTExt ext types
    -> UnionTExt ext (Set.map stripTypeComments $ types)

  BigArrowExt ext from to
    -> BigArrowExt ext from (stripTypeComments to)

  TypeLamExt ext kind typ
    -> TypeLamExt ext kind (stripTypeComments typ)

  TypeAppExt ext x y
    -> TypeAppExt ext (stripTypeComments x) (stripTypeComments y)

  TypeMuExt ext kind itself
    -> TypeMuExt ext kind (stripTypeComments itself)

  TypeBindingExt ext b
    -> TypeBindingExt ext b

  TypeContentBindingExt ext c
    -> TypeContentBindingExt ext c

  TypeSelfBindingExt ext
    -> TypeSelfBindingExt ext

  _ -> error "Non-exhaustive pattern in stripTypeComments"

-- | Strip comments from a case statement.
stripCaseComments
  :: ExprWithoutComments phaseIn phaseOut
  => Case (ExprFor phaseIn) (PatternFor phaseIn)
  -> Case (ExprFor phaseOut) (PatternFor phaseOut)
stripCaseComments (Case expr branches) = Case (stripComments expr) (stripCaseBranchesComments branches)

-- | Strip comments from case branches.
stripCaseBranchesComments
  :: ExprWithoutComments phaseIn phaseOut
  => CaseBranches (ExprFor phaseIn)  (PatternFor phaseIn)
  -> CaseBranches (ExprFor phaseOut) (PatternFor phaseOut)
stripCaseBranchesComments = \case
  DefaultOnly expr
    -> DefaultOnly (stripComments expr)

  CaseBranches neBranches mDefault
    -> CaseBranches (fmap stripCaseBranchComments neBranches) (fmap stripComments mDefault)

-- | Strip comments from a case branch.
stripCaseBranchComments
  :: ExprWithoutComments phaseIn phaseOut
  => CaseBranch (ExprFor phaseIn) (PatternFor phaseIn)
  -> CaseBranch (ExprFor phaseOut) (PatternFor phaseOut)
stripCaseBranchComments (CaseBranch match result) = CaseBranch (stripPatternComments match) (stripComments result)

-- | Strip comments from a pattern.
stripPatternComments
  :: PatternWithoutComments phaseIn phaseOut
  => PatternFor phaseIn
  -> PatternFor phaseOut
stripPatternComments = \case
  FixPhase (PatternExtensionF (Commented _comment commentedPattern))
    -> stripPatternComments commentedPattern

  BindExt ext
    -> BindExt ext

  BindingPatternExt ext b
    -> BindingPatternExt ext b

  SumPatternExt ext sumIndex nestedPattern
    -> SumPatternExt ext sumIndex (stripPatternComments nestedPattern)

  ProductPatternExt ext nestedPatterns
    -> ProductPatternExt ext (fmap stripPatternComments nestedPatterns)

  UnionPatternExt ext unionIndexTy nestedPattern
    -> UnionPatternExt ext (stripTypeComments unionIndexTy) (stripPatternComments nestedPattern)

  _ -> error "Non-exhaustive pattern match when stripping comments from match statement"

-- | Add the ability to comment an expression.
addComments
  :: ExprWithComments phaseIn phaseOut
  => ExprFor phaseIn
  -> ExprFor phaseOut
addComments = \case
  LamExt ext ty expr
    -> LamExt ext (addTypeComments ty) (addComments expr)

  AppExt ext fExpr xExpr
    -> AppExt ext (addComments fExpr) (addComments xExpr)

  CaseAnalysisExt ext c
     -> CaseAnalysisExt ext $ addCaseComments c

  SumExt ext expr ix ty
    -> SumExt ext (addComments expr) ix (fmap addTypeComments ty)

  ProductExt ext prodExprs
    -> ProductExt ext (fmap addComments prodExprs)

  UnionExt ext unionExpr tyIx ty
    -> UnionExt ext (addComments unionExpr) (addTypeComments tyIx) (Set.map addTypeComments ty)

  BindingExt ext b
    -> BindingExt ext b

  ContentBindingExt ext c
    -> ContentBindingExt ext c

  BigLamExt ext takeTy expr
    -> BigLamExt ext takeTy (addComments expr)

  BigAppExt ext fExpr xTy
    -> BigAppExt ext (addComments fExpr) (addTypeComments xTy)

  _ -> error "Non-exhaustive pattern adding comments"

-- | Add the ability to comment a type.
addTypeComments
  :: TypeWithComments phaseIn phaseOut
  => TypeFor phaseIn
  -> TypeFor phaseOut
addTypeComments = \case
  NamedExt ext tyName
    -> NamedExt ext tyName

  ArrowExt ext from to
    -> ArrowExt ext (addTypeComments from) (addTypeComments to)

  SumTExt ext types
    -> SumTExt ext (fmap addTypeComments types)

  ProductTExt ext types
    -> ProductTExt ext (fmap addTypeComments types)

  UnionTExt ext types
    -> UnionTExt ext (Set.map addTypeComments $ types)

  BigArrowExt ext from to
    -> BigArrowExt ext from (addTypeComments to)

  TypeLamExt ext kind typ
    -> TypeLamExt ext kind (addTypeComments typ)

  TypeAppExt ext x y
    -> TypeAppExt ext (addTypeComments x) (addTypeComments y)

  TypeBindingExt ext b
    -> TypeBindingExt ext b

  TypeContentBindingExt ext name
    -> TypeContentBindingExt ext name

  _ -> error "Non-exhaustive pattern in addTypeComments"

-- | Add the ability to comment a case statement.
addCaseComments
  :: ExprWithComments phaseIn phaseOut
  => Case (ExprFor phaseIn) (PatternFor phaseIn)
  -> Case (ExprFor phaseOut) (PatternFor phaseOut)
addCaseComments (Case expr branches) = Case (addComments expr) (addCaseBranchesComments branches)

-- | Add the ability to comment case branches.
addCaseBranchesComments
  :: ExprWithComments phaseIn phaseOut
  => CaseBranches (ExprFor phaseIn) (PatternFor phaseIn)
  -> CaseBranches (ExprFor phaseOut) (PatternFor phaseOut)
addCaseBranchesComments = \case
  DefaultOnly expr
    -> DefaultOnly (addComments expr)

  CaseBranches neBranches mDefault
    -> CaseBranches (fmap addCaseBranchComments neBranches) (fmap addComments mDefault)

-- | Add the ability to comment a case branch.
addCaseBranchComments
  :: ExprWithComments phaseIn phaseOut
  => CaseBranch (ExprFor phaseIn) (PatternFor phaseIn)
  -> CaseBranch (ExprFor phaseOut) (PatternFor phaseOut)
addCaseBranchComments (CaseBranch match result) = CaseBranch (addPatternComments match) (addComments result)

-- | Add the ability to comment a pattern.
addPatternComments
  :: PatternWithComments phaseIn phaseOut
  => PatternFor phaseIn
  -> PatternFor phaseOut
addPatternComments = \case
  BindExt ext
    -> BindExt ext

  BindingPatternExt ext b
    -> BindingPatternExt ext b

  SumPatternExt ext sumIndex nestedPattern
    -> SumPatternExt ext  sumIndex (addPatternComments nestedPattern)

  ProductPatternExt ext  nestedPatterns
    -> ProductPatternExt ext  (fmap addPatternComments nestedPatterns)

  UnionPatternExt ext  unionIndexTy nestedPattern
    -> UnionPatternExt ext  (addTypeComments unionIndexTy) (addPatternComments nestedPattern)

  _ -> error "Non-exhaustive pattern match when add comments from match statement"

