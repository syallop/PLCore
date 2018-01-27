{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MatchArgSpec.Binding
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module MatchArgSpec.Binding
  ( bindingMatchArgTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Grammar.Lispy hiding (appise,lamise,bind)
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

-- Test the binding constructor of MatchArgs.
bindingMatchArgTestCases :: [(Text,MatchArgTestCase)]
bindingMatchArgTestCases =
  [("Binding", bindingMatchArgTestCase)
  ]

-- (One of) the simplest MatchArgs on a binding constructor.
-- Intended to be used in more complex test cases by field substitution.
defaultBindingMatchArgTestCase :: MatchArgTestCase
defaultBindingMatchArgTestCase = MatchArgTestCase
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
    parsesFrom           = "0"

bindingMatchArgTestCase :: MatchArgTestCase
bindingMatchArgTestCase = defaultBindingMatchArgTestCase
