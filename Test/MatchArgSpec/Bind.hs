{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MatchArgSpec.Bind
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module MatchArgSpec.Bind
  ( bindMatchArgTestCases
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

bindMatchArgTestCases :: [(Text,MatchArgTestCase)]
bindMatchArgTestCases =
  [("Bind empty sum"    , bindSumMatchArgTestCase)
  ,("Bind empty product", bindProdMatchArgTestCase)
  ,("Bind named boolean", bindBoolMatchArgTestCase)
  ]

defaultBindMatchArgTestCase :: MatchArgTestCase
defaultBindMatchArgTestCase = MatchArgTestCase
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
    isMatchArg           = Bind
    typed                = SumT []
    checkMatchWithResult = Right [SumT []]
    parsesFrom           = "?"

-- Test binding an empty sum.
bindSumMatchArgTestCase :: MatchArgTestCase
bindSumMatchArgTestCase = defaultBindMatchArgTestCase

-- Test binding an empty product.
bindProdMatchArgTestCase :: MatchArgTestCase
bindProdMatchArgTestCase = defaultBindMatchArgTestCase
  { _typed = ProductT []
  , _checkMatchWithResult = Right [ProductT []]
  }

bindBoolMatchArgTestCase :: MatchArgTestCase
bindBoolMatchArgTestCase = defaultBindMatchArgTestCase
  { _underTypeCtx = fromJust boolTypeCtx
  , _typed = boolTypeName
  , _checkMatchWithResult = Right [boolType]
  }

