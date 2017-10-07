{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec.Lam
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'Lam' constructor.
-}
module ExprSpec.Lam
  ( lamTypeCtx
  , lamTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Kind
import PL.Parser
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))

import ExprTestCase

lamTypeCtx   = insertType "Foo" fooType
             . fromJust
             . insertType "Bar" barType
             . fromJust
             . insertType "Baz" bazType
             $ emptyTypeCtx
fooTypeName = Named "Foo"
fooType     = SumT []
barTypeName = Named "Bar"
barType     = SumT []
bazTypeName = Named "Baz"
bazType     = SumT []

lamTestCases :: [(Text,ExprTestCase)]
lamTestCases =
  [("Single lambda" , singleLamTestCase)
  ,("Nested lambda" , nestedLamTestCase)
  ,("Chained lambda", chainedLamTestCase)
  ]

-- \() -> ()
-- Test a single lambda that takes and returns a specific named type.
-- (Specialised id).
singleLamTestCase :: ExprTestCase
singleLamTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = fromJust lamTypeCtx
    e   = Lam fooTypeName $ Binding VZ
    ty  = Arrow fooType fooType
    txt = "\\Foo 0"

-- \Foo -> \Bar -> Foo
-- Test a nested lambda that takes two different named types and returns the first.
-- (Specialised const).
nestedLamTestCase :: ExprTestCase
nestedLamTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = fromJust lamTypeCtx
    e   = Lam fooTypeName . Lam barTypeName . Binding . VS $ VZ
    ty  = Arrow fooType (Arrow barType fooType)
    txt = "λFoo (λBar 1)"

-- \Foo Bar -> Foo
-- Test a chained lambda that takes two different named types in succession and
-- returns the first.
-- (Specialised const2).
chainedLamTestCase :: ExprTestCase
chainedLamTestCase = nestedLamTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = fromJust lamTypeCtx
    e   = Lam fooTypeName
        . Lam barTypeName
        . Lam bazTypeName
        . Binding
        . VS
        . VS
        $ VZ
    ty  = Arrow fooType
        . Arrow barType
        . Arrow bazType
        $ fooType
    txt = "λFoo Bar Baz 2"

