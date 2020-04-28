{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Pattern.Binding
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.Test.Pattern.Binding
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
import PL.Pattern

import Data.Text (Text)
import Data.Maybe (fromJust)

import PL.Test.PatternTestCase

import PL.Test.Expr.Boolean
import PL.Test.Source

data TestBindingSources = TestBindingSources
  { _bindingTestCase :: Source
  }

-- Test the binding constructor of Patterns.
bindingTestCases
  :: TestBindingSources
  -> [(Text, PatternTestCase)]
bindingTestCases t =
  [("Binding", bindingPatternTestCase . _bindingTestCase $ t)
  ]

-- (One of) the simplest Patterns on a binding constructor.
-- Intended to be used in more complex test cases by field substitution.
defaultBindingPatternTestCase
  :: Source
  -> PatternTestCase
defaultBindingPatternTestCase src
  = PatternTestCase
      {_underTypeCtx         = typeCtx
      ,_underExprBindCtx     = exprBindCtx
      ,_underTypeBindCtx     = typeBindCtx
      ,_underTypeBindings    = typeBindings
      ,_isPattern           = isPattern
      ,_typed                = typed
      ,_checkMatchWithResult = checkMatchWithResult
      ,_parsesFrom           = parsesFrom
      }
  where
    typeCtx              = emptyTypeCtx
    exprBindCtx          = addBinding EmptyProductT $ emptyCtx
    typeBindCtx          = emptyCtx
    typeBindings         = emptyBindings
    isPattern           = BindingPattern VZ
    typed                = EmptyProductT
    checkMatchWithResult = Right []
    parsesFrom           = src

bindingPatternTestCase
  :: Source
  -> PatternTestCase
bindingPatternTestCase
  = defaultBindingPatternTestCase

