{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.MatchArg.Sum
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.MatchArg.Sum
  ( TestSumSources (..)
  , sumTestCases
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

import PLParser

import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE

import PL.Test.MatchArgTestCase

import PL.Test.Expr.Boolean
import PL.Test.Source

data TestSumSources = TestSumSources
  { _sumTestCase :: Source
  }

-- Test the sum constructor of MatchArgs.
sumTestCases
  :: TestSumSources
  -> [(Text,MatchArgTestCase)]
sumTestCases t =
  [("Empty sum", sumMatchArgTestCase . _sumTestCase $ t)
  ]

-- (One of) the simplest MatchArgs on a sum constructor. Intended to be used in
-- more complex test cases by field substitution.
defaultSumMatchArgTestCase
  :: Source
  -> MatchArgTestCase
defaultSumMatchArgTestCase src
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

    -- MatchArg might not support matching on empty sums.
    -- One of the simplest patterns is therefore a single sum of an empty
    -- product.
    isMatchArg           = MatchSum 0 (MatchProduct [])
    typed                = SumT $ NE.fromList [ProductT []]
    checkMatchWithResult = Right []
    parsesFrom           = src

sumMatchArgTestCase
  :: Source
  -> MatchArgTestCase
sumMatchArgTestCase
  = defaultSumMatchArgTestCase

