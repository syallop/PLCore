{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MatchArgSpec.Sum
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module MatchArgSpec.Sum
  ( sumMatchArgTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Kind
import PL.PLParser.Parser
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import Data.Text (Text)
import Data.Maybe (fromJust)

import MatchArgTestCase

import ExprSpec.Boolean

-- Test the sum constructor of MatchArgs.
sumMatchArgTestCases :: [(Text,MatchArgTestCase)]
sumMatchArgTestCases =
  [--("Empty sum", sumMatchArgTestCase)
  ]

-- (One of) the simplest MatchArgs on a sum constructor. Intended to be used in
-- more complex test cases by field substitution.
defaultSumMatchArgTestCase :: MatchArgTestCase
defaultSumMatchArgTestCase = MatchArgTestCase
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

    -- MatchArg might not support matching on empty sums.
    -- One of the simplest patterns is therefore a single sum of an empty
    -- product.
    isMatchArg           = MatchSum 0 (MatchProduct [])
    typed                = SumT [ProductT []]
    checkMatchWithResult = Right []
    parsesFrom           = "(+ (*))"

sumMatchArgTestCase :: MatchArgTestCase
sumMatchArgTestCase = defaultSumMatchArgTestCase
