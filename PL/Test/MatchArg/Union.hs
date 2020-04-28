{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.MatchArg.Union
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.MatchArg.Union
  (TestUnionSources (..)
  , unionTestCases
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

import Data.Maybe (fromJust)
import Data.Text (Text)

import qualified Data.Set as Set

import PL.Test.MatchArgTestCase

import PL.Test.Expr.Boolean
import PL.Test.Source

data TestUnionSources = TestUnionSources
  { _unionTestCase :: Source
  }

-- Test the union constructor of MatchArgs.
unionTestCases
  :: TestUnionSources
  -> [(Text, MatchArgTestCase)]
unionTestCases t =
  [("Single union", unionMatchArgTestCase . _unionTestCase $ t)
  ]

-- A simple MatchArg on a union constructor.
-- Intended to be used in more complex test cases by field substitution.
defaultUnionMatchArgTestCase
  :: Source
  -> MatchArgTestCase
defaultUnionMatchArgTestCase src
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

    isMatchArg           = MatchUnion EmptyProductT MatchEmptyProduct
    typed                = UnionT $ Set.fromList $ [EmptyProductT]
    checkMatchWithResult = Right []
    parsesFrom           = src

unionMatchArgTestCase
  :: Source
  -> MatchArgTestCase
unionMatchArgTestCase
  = defaultUnionMatchArgTestCase

