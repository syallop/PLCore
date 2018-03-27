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
import PL.FixExpr
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.FixType
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

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
fooTypeName = fixType $ Named "Foo"
fooType     = fixType $ SumT []
barTypeName = fixType $ Named "Bar"
barType     = fixType $ SumT []
bazTypeName = fixType $ Named "Baz"
bazType     = fixType $ SumT []

lamTestCases :: [(Text,ExprTestCase)]
lamTestCases =
  [("Single lambda" , singleLamTestCase)
  ,("Nested lambda" , nestedLamTestCase)
  {-,("Chained lambda", chainedLamTestCase)-} -- TODO: Re-enable chaining
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
    e   = fixExpr $ Lam fooTypeName $ fixExpr $ Binding VZ
    ty  = fixType $ Arrow fooType fooType
    txt = "λFoo (0)"

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
    e   = fixExpr $ Lam fooTypeName . fixExpr . Lam barTypeName . fixExpr . Binding . VS $ VZ
    ty  = fixType $ Arrow fooType (fixType $ Arrow barType fooType)
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
    e   = fixExpr
        . Lam fooTypeName
        . fixExpr
        . Lam barTypeName
        . fixExpr
        . Lam bazTypeName
        . fixExpr
        . Binding
        . VS
        . VS
        $ VZ
    ty  = fixType
        . Arrow fooType
        . fixType
        . Arrow barType
        . fixType
        . Arrow bazType
        $ fooType
    txt = "λFoo Bar Baz 2"

