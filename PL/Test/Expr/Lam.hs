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
import PL.TypeCheck
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
  ]

-- Test a single lambda that takes and returns a specific named type.
-- (Specialised id).
singleLamTestCase
  :: Source
  -> ExprTestCase
singleLamTestCase src
  = ExprTestCase
  { _parsesFrom   = src
  , _parsesTo     = Lam boolTypeName $ Binding VZ

  , _underResolveCtx = undefined
  , _resolvesTo      = Lam boolTypeName $ Binding VZ

  , _underTypeCheckCtx = topTypeCheckCtx ctx
  , _typed             = Arrow boolType boolType

  , _underReductionCtx    = topReductionCtx ctx
  , _reducesTo            = Lam boolTypeName $ Binding VZ
  , _reducesToWhenApplied =
      [ ( "true ~> true"
        , [(`App` trueTerm)
          ]
        , Just trueTerm
        )
      , ( "false ~> false"
        , [(`App` falseTerm)
          ]
        , Just falseTerm
        )
      ]
  }
  where
    ctx = boolTypeCtx <> natTypeCtx

-- \Bool -> \Nat -> Bool
-- Test a nested lambda that takes two different named types and returns the first.
-- (Specialised const).
nestedLamTestCase
  :: Source
  -> ExprTestCase
nestedLamTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam boolTypeName . Lam natTypeName . Binding . VS $ VZ

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam boolTypeName . Lam natTypeName . Binding . VS $ VZ

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed             = Arrow boolType (Arrow natType boolType)

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo         = Lam boolTypeName . Lam natTypeName . Binding . VS $ VZ
      , _reducesToWhenApplied =
          [ ("Reduce to outer value"
            , [ (`App` trueTerm)
              , (`App` zero)
              ]
            , Just trueTerm
            )

          , ("Reduce under lambda"
            , [ (`App` trueTerm)
              ]
            , Just $ Lam natTypeName trueTerm
            )
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = boolTypeCtx <> natTypeCtx

