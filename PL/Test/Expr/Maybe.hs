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
defaultNatTestCase
  :: Source
  -> ExprTestCase
defaultNatTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      ,_reducesTo = stripComments e
      ,_reducesToWhenApplied = reductions
      }
  where
    ctx = maybeTypeCtx <> natTypeCtx

    -- \x:Maybe Nat ->
    -- case x of
    --   Nothing -> 0
    --   Just n  -> n
    e   = Lam (TypeApp maybeTypeName natTypeName) $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
            (NE.fromList
              [ CaseBranch nothingPat zero
              , CaseBranch justPat (Binding VZ)
              ]
            )
            Nothing

    ty  = Arrow (TypeApp maybeTypeName natTypeName) natTypeName

    reductions =
      [ ( "Default to 0"
        , [BigApp nothingTerm natType]
        , zero
        )

      , ( "Extract 1"
        , [App (BigApp justTerm natType) one]
        , one
        )
      ]

