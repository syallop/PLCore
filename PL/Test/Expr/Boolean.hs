{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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

data TestBooleanSources = TestBooleanSources
  { _andTestCase :: Source
  }

booleanTestCases
  :: TestBooleanSources
  -> [(Text, ExprTestCase)]
booleanTestCases t =
  [("and", andExprTestCase . _andTestCase $ t)
  ]

boolTypeCtx
  :: (SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => TypeCtx phase
boolTypeCtx = fromJust $ insertType "Bool" boolType emptyTypeCtx

boolTypeName
  :: NamedExtension phase ~ Void
  => TypeFor phase
boolTypeName = Named "Bool"

boolType
  :: (SumTExtension phase ~ Void, ProductTExtension phase ~ Void)
  => TypeFor phase
boolType = SumT boolSumType

boolSumType
  :: (SumTExtension phase ~ Void, ProductTExtension phase ~ Void)
  => NonEmpty (TypeFor phase)
boolSumType = NE.fromList $
  [ ProductT []
  , ProductT []
  ]

falseTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => ExprFor phase
falseTerm = Sum (Product []) 0 boolSumType

trueTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => ExprFor phase
trueTerm = Sum (Product []) 1 boolSumType

falsePat
  :: (MatchSumExtension phase     ~ Void
     ,MatchProductExtension phase ~ Void
     )
  => MatchArgFor phase
falsePat = MatchSum 0 (MatchProduct [])

truePat
  :: (MatchSumExtension phase     ~ Void
     ,MatchProductExtension phase ~ Void
     )
  => MatchArgFor phase
truePat = MatchSum 1 (MatchProduct [])

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

      ,_reducesTo = stripComments e
      ,_reducesToWhenApplied = reductions
      }
  where
    ctx = boolTypeCtx
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

