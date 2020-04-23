{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.MatchArg.Bind
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.MatchArg.Bind
  ( TestBindSources (..)
  , bindTestCases
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

data TestBindSources = TestBindSources
  { _bindEmptySum     :: Source
  , _bindEmptyProduct :: Source
  , _bindNamedBoolean :: Source
  }

bindTestCases
  :: TestBindSources
  -> [(Text, MatchArgTestCase)]
bindTestCases t =
  [("Bind empty sum"    , bindSumMatchArgTestCase  . _bindEmptySum     $ t)
  ,("Bind empty product", bindProdMatchArgTestCase . _bindEmptyProduct $ t)
  ,("Bind named boolean", bindBoolMatchArgTestCase . _bindNamedBoolean $ t)
  ]

defaultBindMatchArgTestCase
  :: Source
  -> MatchArgTestCase
defaultBindMatchArgTestCase src
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
    isMatchArg           = Bind
    typed                = SumT $ NE.fromList [ProductT []]
    checkMatchWithResult = Right [SumT $ NE.fromList [ProductT []]]
    parsesFrom           = src

-- Test binding an empty sum.
bindSumMatchArgTestCase
  :: Source
  -> MatchArgTestCase
bindSumMatchArgTestCase = defaultBindMatchArgTestCase

-- Test binding an empty product.
bindProdMatchArgTestCase
  :: Source
  -> MatchArgTestCase
bindProdMatchArgTestCase src = (defaultBindMatchArgTestCase src)
  { _typed = ProductT []
  , _checkMatchWithResult = Right [ProductT []]
  }

bindBoolMatchArgTestCase
  :: Source
  -> MatchArgTestCase
bindBoolMatchArgTestCase src = (defaultBindMatchArgTestCase src)
  { _underTypeCtx = fromJust boolTypeCtx
  , _typed = boolTypeName
  , _checkMatchWithResult = Right [boolType]
  }

