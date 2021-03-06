{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PL.Test.ExprSpec where

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
import PL.Test.Expr

import PLPrinter

import Data.Text
import qualified Data.Text as Text
import qualified Data.Map as Map

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
      , _selfTypeTestCases = errNoSource
      }

    errNoSource = error "Core tests do not define syntax and so do not test parsing"

    ppDefaultError = PPError
      { _ppExpr        = ppExpr
      , _ppType        = ppType
      , _ppPattern     = ppPattern
      , _ppKind        = ppKind
      , _ppTypeCtx     = ppTypeCtx document ppType
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

