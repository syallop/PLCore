{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.Function
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.ExprCase
  (
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

import PL.Test.ExprTestCase
import PL.Test.Source

bindExprTestCase
  :: Source
  -> ExprTestCase
bindExprTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _parsesTo       = e
      , _typed        = ty
      , _parsesFrom   = src
      }
  where
    ctx = emptyTypeCtx
    e   = 
    ty  =
    src = "?"

