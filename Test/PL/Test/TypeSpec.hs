{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module PL.Test.TypeSpec where

import PL.Error
import PL.Expr
import PL.Kind
import PL.TyVar
import PL.Pattern
import PL.Type
import PL.Var
import PL.TypeCtx

import PL.Test.TypeTestCase
import PL.Test.Type

import PLPrinter

import Data.Text
import qualified Data.Text as Text
import qualified Data.Map as Map

import Test.Hspec

spec
  :: Spec
spec = do
  describe "Types" $ do
    describe "Type (kind) check" $ typeChecksTypesSpec typeTestCases ppDefaultError
    describe "Reduce"            $ reducesTypesToSpec  typeTestCases ppDefaultError
  where
    -- We only want to test type-checking and reduction as we don't define a
    -- parsable syntax at this level. The test function demands source so..
    typeTestCases :: Map.Map Text TypeTestCase
    typeTestCases = mkTypeTestCases $ TestTypeSources
      { _namedTestCases       = errNoSource
      , _arrowTestCases       = errNoSource
      , _sumTestCases         = errNoSource
      , _productTestCases     = errNoSource
      , _unionTestCases       = errNoSource
      , _bigArrowTestCases    = errNoSource
      , _typeLamTestCases     = errNoSource
      , _typeBindingTestCases = errNoSource
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

    ppExpr :: forall phase. Show (ExprFor phase) => ExprFor phase -> Doc
    ppExpr = text . Text.pack . show

    ppType :: Type -> Doc
    ppType = text . Text.pack . show

    ppKind :: Kind -> Doc
    ppKind = text . Text.pack . show

    ppPattern :: Pattern -> Doc
    ppPattern = text . Text.pack . show

    ppVar :: Var -> Doc
    ppVar = text . Text.pack . show

    ppTyVar :: TyVar -> Doc
    ppTyVar = text . Text.pack . show

