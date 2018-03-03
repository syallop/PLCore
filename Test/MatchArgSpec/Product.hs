{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MatchArgSpec.Product
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module MatchArgSpec.Product
  ( productMatchArgTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.FixType
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

import Data.Text (Text)
import Data.Maybe (fromJust)

import MatchArgTestCase

import ExprSpec.Boolean

-- Test the product constructor of MatchArgs.
productMatchArgTestCases :: [(Text,MatchArgTestCase)]
productMatchArgTestCases =
  [("Empty product", productMatchArgTestCase)
  ]

-- The simplest MatchArg on a product constructor is the empty product.
-- Intended to be used in
-- more complex test cases by field substitution.
defaultProductMatchArgTestCase :: MatchArgTestCase
defaultProductMatchArgTestCase = MatchArgTestCase
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

    isMatchArg           = MatchProduct []
    typed                = fixType $ ProductT []
    checkMatchWithResult = Right []
    parsesFrom           = "(*)"

productMatchArgTestCase :: MatchArgTestCase
productMatchArgTestCase = defaultProductMatchArgTestCase
