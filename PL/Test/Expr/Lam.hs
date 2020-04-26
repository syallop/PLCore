{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.Lam
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'Lam' constructor.
-}
module PL.Test.Expr.Lam
  ( lamTypeCtx
  , lamTestCases
  , TestLamSources (..)
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Commented
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

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.ExprTestCase
import PL.Test.Source

-- A record of the sources required to run all the TestLamSources tests.
data TestLamSources = TestLamSources
  { _singleLamTestCase  :: Source
  , _nestedLamTestCase  :: Source
  , _chainedLamTestCase :: Source
  }

lamTestCases
  :: TestLamSources
  -> [(Text,ExprTestCase)]
lamTestCases t =
  [ ("Single lambda" , singleLamTestCase . _singleLamTestCase $ t)
  , ("Nested lambda" , nestedLamTestCase . _nestedLamTestCase $ t)
  {-,("Chained lambda", chainedLamTestCase)-} -- TODO: Re-enable chaining
  ]

lamTypeCtx
  = insertType "Foo" fooType
  . fromJust
  . insertType "Bar" barType
  . fromJust
  . insertType "Baz" bazType
  $ emptyTypeCtx
fooTypeName = Named "Foo"
fooType     = SumT $ NE.fromList [ProductT []]
barTypeName = Named "Bar"
barType     = SumT $ NE.fromList [ProductT []]
bazTypeName = Named "Baz"
bazType     = SumT $ NE.fromList [ProductT []]


-- \() -> ()
-- Test a single lambda that takes and returns a specific named type.
-- (Specialised id).
singleLamTestCase
  :: Source
  -> ExprTestCase
singleLamTestCase src
  = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = src

  ,_reducesTo            = stripComments e
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = fromJust lamTypeCtx
    e   = Lam fooTypeName $ Binding VZ
    ty  = Arrow fooType fooType

    -- TODO
    reduces = []

-- \Foo -> \Bar -> Foo
-- Test a nested lambda that takes two different named types and returns the first.
-- (Specialised const).
nestedLamTestCase
  :: Source
  -> ExprTestCase
nestedLamTestCase src
  = ExprTestCase
      {_underTypeCtx = ctx
      ,_isExpr       = e
      ,_typed        = ty
      ,_parsesFrom   = src

      ,_reducesTo = stripComments e
      ,_reducesToWhenApplied = reduces
      }
  where
    ctx = fromJust lamTypeCtx
    e   = Lam fooTypeName . Lam barTypeName . Binding . VS $ VZ
    ty  = Arrow fooType (Arrow barType fooType)

    -- TODO
    reduces = []

-- \Foo Bar -> Foo
-- Test a chained lambda that takes two different named types in succession and
-- returns the first.
-- (Specialised const2).
chainedLamTestCase
  :: Source
  -> ExprTestCase
chainedLamTestCase src
  = (nestedLamTestCase src)
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      ,_reducesTo = stripComments e
      ,_reducesToWhenApplied = reduces
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

    -- TODO
    reduces = []

