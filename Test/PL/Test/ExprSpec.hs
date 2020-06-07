{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PL.Test.ExprSpec where

import PL
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.TyVar
import PL.FixPhase
import PL.Type
import PL.Var
import PL.Pattern
import PL.TypeCtx

import PL.Test.ExprTestCase
import PL.Test.Parsing.Expr
import PL.Test.Expr
import PL.Test.Expr.BigLam
import PL.Test.Expr.Maybe
import PL.Test.Expr.Boolean
import PL.Test.Expr.Function
import PL.Test.Expr.Lam
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union
import PL.Test.Source
import PL.Test.Util

import PLGrammar
import PLPrinter
import PLPrinter.Doc

import Control.Monad
import Data.Text
import qualified Data.Text as Text
import Data.Monoid hiding (Product, Sum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.List as List

import Test.Hspec

spec
  :: Spec
spec = do
  describe "Expressions" $ do
    describe "Type check" $ typeChecksSpec exprTestCases ppDefaultError
    describe "Reduce"     $ reducesToSpec  exprTestCases ppDefaultError
  where
    -- We only want to test type-checking and reduction as we don't define a
    -- parsable syntax at this level. The test function demands source so..
    exprTestCases :: Map.Map Text ExprTestCase
    exprTestCases = mkTestCases $ TestExprSources
      { _lamTestCases      = errNoSource
      , _bigLamTestCases   = errNoSource
      , _booleanTestCases  = errNoSource
      , _bindingTestCases  = errNoSource
      , _naturalTestCases  = errNoSource
      , _sumTestCases      = errNoSource
      , _productTestCases  = errNoSource
      , _unionTestCases    = errNoSource
      , _functionTestCases = errNoSource
      , _maybeTestCases    = errNoSource
      , _listTestCases     = errNoSource
      }

    errNoSource = error "Core tests do not define syntax and so do not test parsing"

    ppDefaultError = PPError
      { _ppExpr        = ppExpr
      , _ppType        = ppType
      , _ppPattern     = ppPattern
      , _ppKind        = ppKind
      , _ppTypeCtx     = ppTypeCtx document (ppTypeInfo ppType)
      , _ppTypeName    = document
      , _ppBinding     = ppVar
      , _ppTypeBinding = ppTyVar
      }

    ppExpr :: ExprFor DefaultPhase -> Doc
    ppExpr = text . Text.pack . show

    ppType :: TypeFor DefaultPhase -> Doc
    ppType = text . Text.pack . show

    ppKind :: Kind -> Doc
    ppKind = text . Text.pack . show

    ppPattern :: PatternFor DefaultPhase -> Doc
    ppPattern = text . Text.pack . show

    ppVar :: Var -> Doc
    ppVar = text . Text.pack . show

    ppTyVar :: TyVar -> Doc
    ppTyVar = text . Text.pack . show

