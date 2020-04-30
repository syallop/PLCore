{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.Lam
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'Lam' constructor.
-}
module PL.Test.Expr.Lam
  ( lamTestCases
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

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

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
    ctx = boolTypeCtx <> natTypeCtx
    e   = Lam boolTypeName $ Binding VZ
    ty  = Arrow boolType boolType

    reduces =
      [ ( "true ~> true"
        , [(`App` trueTerm)
          ]
        , trueTerm
        )
      , ( "false ~> false"
        , [(`App` falseTerm)
          ]
        , falseTerm
        )
      ]

-- \Bool -> \Nat -> Bool
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
    ctx = boolTypeCtx <> natTypeCtx
    e   = Lam boolTypeName . Lam natTypeName . Binding . VS $ VZ
    ty  = Arrow boolType (Arrow natType boolType)

    reduces =
      [ ("Reduce to outer value"
        , [ (`App` trueTerm)
          , (`App` zero)
          ]
        , trueTerm
        )

      , ("Reduce under lambda"
        , [ (`App` trueTerm)
          ]
        , Lam natTypeName trueTerm
        )
      ]

-- \Bool Nat -> Unit
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
    ctx = boolTypeCtx <> natTypeCtx
    e   = Lam boolTypeName
        . Lam natTypeName
        . Lam unitTypeName
        . Binding
        . VS
        . VS
        $ VZ
    ty  = Arrow boolType
        . Arrow natType
        . Arrow unitType
        $ boolType

    -- TODO
    reduces = []

