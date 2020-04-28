{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.MatchArg.Product
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.MatchArg.Product
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
import PL.TypeCtx
import PL.Var

import Data.Text (Text)
import Data.Maybe (fromJust)

import PL.Test.MatchArgTestCase

import PL.Test.Expr.Boolean
import PL.Test.Source

data TestProductSources = TestProductSources
  { _productTestCase :: Source
  }

-- Test the product constructor of MatchArgs.
productTestCases
  :: TestProductSources
  -> [(Text, MatchArgTestCase)]
productTestCases t =
  [("Empty product", productMatchArgTestCase . _productTestCase $ t)
  ]

-- The simplest MatchArg on a product constructor is the empty product.
-- Intended to be used in
-- more complex test cases by field substitution.
defaultProductMatchArgTestCase
  :: Source
  -> MatchArgTestCase
defaultProductMatchArgTestCase src
  = MatchArgTestCase
      {_underTypeCtx         = typeCtx
      ,_underExprBindCtx     = exprBindCtx
      ,_underTypeBindCtx     = typeBindCtx
      ,_underTypeBindings    = typeBindings
      ,_isMatchArg           = isMatchArg
      ,_typed                = typed
      ,_checkMatchWithResult = checkMatchWithResult
      ,_parsesFrom           = parsesFrom
      }
  where
    typeCtx              = emptyTypeCtx
    exprBindCtx          = emptyCtx
    typeBindCtx          = emptyCtx
    typeBindings         = emptyBindings

    isMatchArg           = MatchEmptyProduct
    typed                = EmptyProductT
    checkMatchWithResult = Right []
    parsesFrom           = src

productMatchArgTestCase
  :: Source
  -> MatchArgTestCase
productMatchArgTestCase
  = defaultProductMatchArgTestCase

