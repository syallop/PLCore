{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Text.Expr.Boolean
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a 'Boolean' type.
-}
module PL.Test.Expr.Boolean
  ( boolTypeCtx
  , boolTypeName
  , boolType
  , boolSumType
  , falseTerm
  , trueTerm
  , falsePat
  , truePat

  , andExprTestCase

  , TestBooleanSources (..)
  , booleanTestCases
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

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.ExprTestCase
import PL.Test.Source

data TestBooleanSources = TestBooleanSources
  { _andTestCase :: Source
  }

booleanTestCases
  :: TestBooleanSources
  -> [(Text, ExprTestCase)]
booleanTestCases t =
  [("and", andExprTestCase . _andTestCase $ t)
  ]

boolTypeCtx = insertType "Bool" boolType emptyTypeCtx
boolTypeName = Named "Bool"
boolType = SumT boolSumType
boolSumType = NE.fromList $
                [ProductT []
                ,ProductT []
                ]
falseTerm     = Sum (Product []) 0 boolSumType
trueTerm      = Sum (Product []) 1 boolSumType
falsePat      = MatchSum 0 (MatchProduct [])
truePat       = MatchSum 1 (MatchProduct [])

-- Boolean and
andExprTestCase
  :: Source
  -> ExprTestCase
andExprTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      ,_reducesTo = e
      ,_reducesToWhenApplied = reductions
      }
  where
    ctx = fromJust boolTypeCtx
    e   = Lam boolTypeName $ Lam boolTypeName $                               -- \x:Bool y:Bool ->
        CaseAnalysis $ Case (Binding VZ)                                      -- case y of
          $ CaseBranches                                                      --
            (CaseBranch falsePat falseTerm :| []                              --     False -> False
            )                                                                 --
            (Just                                                             --     _      ->
                (CaseAnalysis $ Case (Binding $ VS VZ)                        --               case x of
                  $ CaseBranches                                              --
                    (CaseBranch falsePat falseTerm :|[]                       --                 False -> False
                    )                                                         --
                    (Just                                                     --                        _     ->
                        trueTerm                                              --                                 True
                    )
                )
            )
    ty  = Arrow boolType (Arrow boolType boolType)

    reductions =
      [ ("true"
        , [trueTerm]
        , Lam (Named "Bool") $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
            (CaseBranch falsePat falseTerm :| [])
            (Just trueTerm)
        )

      , ("false true"
        , [falseTerm, trueTerm]
        , falseTerm
        )

      , ("true true"
        , [trueTerm, trueTerm]
        , trueTerm
        )
      ]

