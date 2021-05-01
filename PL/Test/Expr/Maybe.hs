{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Text.Expr.Maybe
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a 'Maybe' type.
-}
module PL.Test.Expr.Maybe
  ( TestMaybeSources (..)
  , maybeTestCases
  , defaultNatTestCase
  )
  where

import PL.Case
import PL.Expr
import PL.Reduce
import PL.Type
import PL.TypeCheck
import PL.Var

import Data.Text (Text)
import qualified Data.List.NonEmpty as NE

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared


data TestMaybeSources = TestMaybeSources
  { _defaultNatTestCase :: Source
  }

maybeTestCases
  :: TestMaybeSources
  -> [(Text, ExprTestCase)]
maybeTestCases t =
  [("default nat", defaultNatTestCase . _defaultNatTestCase $ t)
  ]

-- Extract a Natural number from a Maybe with a default value.
--
-- \x:Maybe Nat ->
-- case x of
--   Nothing -> 0
--   Just n  -> n
defaultNatTestCase
  :: Source
  -> ExprTestCase
defaultNatTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   =
          Lam (TypeApp maybeTypeName natTypeName) $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
            (NE.fromList
              [ CaseBranch nothingPat zero
              , CaseBranch justPat (Binding VZ)
              ]
            )
            Nothing

      , _underResolveCtx = undefined
      , _resolvesTo      =
          Lam (TypeApp maybeTypeName natTypeName) $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
            (NE.fromList
              [ CaseBranch nothingPat zero
              , CaseBranch justPat (Binding VZ)
              ]
            )
            Nothing

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed             = Arrow (TypeApp maybeTypeName natTypeName) natTypeName

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo          =
          Lam (TypeApp maybeTypeName natTypeName) $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
            (NE.fromList
              [ CaseBranch nothingPat zero
              , CaseBranch justPat (Binding VZ)
              ]
            )
            Nothing
      , _reducesToWhenApplied =
          [ ( "Default to 0"
            , [(`App` (BigApp nothingTerm natType))]
            , Just zero
            )

          , ( "Extract 1"
            , [(`App` (App (BigApp justTerm natType) one))]
            , Just one
            )
          ]

      , _underEvaluationCtx     = undefined
      , _evaluatesTo            = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = maybeTypeCtx <> natTypeCtx


