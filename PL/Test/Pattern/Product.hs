{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Pattern.Product
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.Pattern.Product
  ( TestProductSources (..)
  , productTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.Pattern
import PL.TypeCtx
import PL.Var

import Data.Text (Text)
import Data.Maybe (fromJust)

import PL.Test.PatternTestCase

import PL.Test.Expr.Boolean
import PL.Test.Source

data TestProductSources = TestProductSources
  { _productTestCase :: Source
  }

-- Test the product constructor of Patterns.
productTestCases
  :: TestProductSources
  -> [(Text, PatternTestCase)]
productTestCases t =
  [("Empty product", productPatternTestCase . _productTestCase $ t)
  ]

-- The simplest Pattern on a product constructor is the empty product.
-- Intended to be used in
-- more complex test cases by field substitution.
defaultProductPatternTestCase
  :: Source
  -> PatternTestCase
defaultProductPatternTestCase src
  = PatternTestCase
      {_underTypeCtx         = typeCtx
      ,_underExprBindCtx     = exprBindCtx
      ,_underTypeBindCtx     = typeBindCtx
      ,_underTypeBindings    = typeBindings
      ,_isPattern           = isPattern
      ,_typed                = typed
      ,_checkMatchWithResult = checkMatchWithResult
      ,_parsesFrom           = parsesFrom
      }
  where
    typeCtx              = emptyTypeCtx
    exprBindCtx          = emptyCtx
    typeBindCtx          = emptyCtx
    typeBindings         = emptyBindings

    isPattern           = EmptyProductPattern
    typed                = EmptyProductT
    checkMatchWithResult = Right []
    parsesFrom           = src

productPatternTestCase
  :: Source
  -> PatternTestCase
productPatternTestCase
  = defaultProductPatternTestCase
