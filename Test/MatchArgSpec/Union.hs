{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MatchArgSpec.Union
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module MatchArgSpec.Union
  ( unionMatchArgTestCases
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
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

import Data.Maybe (fromJust)
import Data.Text (Text)

import qualified Data.Set as Set

import MatchArgTestCase

import ExprSpec.Boolean

-- Test the union constructor of MatchArgs.
unionMatchArgTestCases :: [(Text,MatchArgTestCase)]
unionMatchArgTestCases =
  [--("Single union", unionMatchArgTestCase)
  ]

-- A simple MatchArg on a union constructor.
-- Intended to be used in more complex test cases by field substitution.
defaultUnionMatchArgTestCase :: MatchArgTestCase
defaultUnionMatchArgTestCase = MatchArgTestCase
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

    isMatchArg           = MatchUnion (ProductT []) (MatchProduct [])
    typed                = UnionT $ Set.fromList $ [ProductT []]
    checkMatchWithResult = Right []
    parsesFrom           = "∪ (*)"

unionMatchArgTestCase :: MatchArgTestCase
unionMatchArgTestCase = defaultUnionMatchArgTestCase
