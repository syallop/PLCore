{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.MatchArg.Binding
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.Test.MatchArg.Binding
  ( TestBindingSources (..)
  , bindingTestCases
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

data TestBindingSources = TestBindingSources
  { _bindingTestCase :: Source
  }

-- Test the binding constructor of MatchArgs.
bindingTestCases
  :: TestBindingSources
  -> [(Text, MatchArgTestCase)]
bindingTestCases t =
  [("Binding", bindingMatchArgTestCase . _bindingTestCase $ t)
  ]

-- (One of) the simplest MatchArgs on a binding constructor.
-- Intended to be used in more complex test cases by field substitution.
defaultBindingMatchArgTestCase
  :: Source
  -> MatchArgTestCase
defaultBindingMatchArgTestCase src
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
    exprBindCtx          = addBinding (ProductT []) $ emptyCtx
    typeBindCtx          = emptyCtx
    typeBindings         = emptyBindings
    isMatchArg           = MatchBinding VZ
    typed                = ProductT []
    checkMatchWithResult = Right []
    parsesFrom           = src

bindingMatchArgTestCase
  :: Source
  -> MatchArgTestCase
bindingMatchArgTestCase
  = defaultBindingMatchArgTestCase

