{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-|
Module      : PL.Test.Expr.Natural
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a 'Natural' type.
-}
module PL.Test.Expr.Natural
  ( natTypeCtx
  , natTypeName
  , natType
  , natSumType
  , zTerm
  , sTerm
  , zPat
  , sPat

  , zero
  , succ
  , one
  , two
  , three
  , four

  , subTwoExprTestCase

  , TestNaturalSources (..)
  , naturalTestCases
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

import Data.Text (Text)
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text

import Data.Monoid ((<>))

import PL.Test.ExprTestCase
import PL.Test.Source

data TestNaturalSources = TestNaturalSources
  { _subTwoTestCase :: Source
  }

naturalTestCases
  :: TestNaturalSources
  -> [(Text, ExprTestCase)]
naturalTestCases t =
  [ ("subtract two", subTwoExprTestCase . _subTwoTestCase $ t)
  ]


natTypeCtx
  :: (SumTExtension phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => TypeCtx phase
natTypeCtx = fromJust $ insertRecType "Nat" natType emptyTypeCtx

natTypeName
  :: NamedExtension phase ~ Void
  => TypeFor phase
natTypeName = Named "Nat"

natType
  :: (SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => TypeFor phase
natType = SumT natSumType

natSumType
  :: (ProductTExtension phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => NonEmpty (TypeFor phase)
natSumType = NE.fromList $
  [ProductT []
  ,Named "Nat"
  ]

zTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension phase ~ Void
     )
  => ExprFor phase
zTerm = Sum (Product []) 0 natSumType

sTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,ProductTExtension phase ~ Void
     ,LamExtension      phase ~ Void
     ,BindingFor        phase ~ Var
     ,AbstractionFor    phase ~ TypeFor phase
     ,BindingExtension  phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => ExprFor phase
sTerm = Lam (Named "Nat") $ Sum (Binding (mkVar 0)) 1 natSumType

zPat
  :: (MatchSumExtension     phase ~ Void
     ,MatchProductExtension phase ~ Void
     )
  => MatchArgFor phase
zPat = MatchSum 0 (MatchProduct [])

sPat
  :: (MatchSumExtension phase ~ Void)
  => MatchArgFor phase
  -> MatchArgFor phase
sPat = MatchSum 1

type SucConstraints phase =
  (AppExtension      phase ~ Void
  ,BindingExtension  phase ~ Void
  ,AbstractionFor    phase ~ TypeFor phase
  ,BindingFor        phase ~ Var
  ,LamExtension      phase ~ Void
  ,ProductExtension  phase ~ Void
  ,ProductTExtension phase ~ Void
  ,SumExtension      phase ~ Void
  ,NamedExtension    phase ~ Void
  )

suc
  :: SucConstraints phase
  => ExprFor phase
  -> ExprFor phase
suc n = App sTerm (n)

zero, one, two, three, four :: SucConstraints phase => ExprFor phase
zero  = zTerm
one   = suc zero
two   = suc one
three = suc two
four  = suc three

-- Test nested pattern matching
-- n > 2     ~> n-2
-- otherwise ~> 0
subTwoExprTestCase
  :: Source
  -> ExprTestCase
subTwoExprTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      , _reducesTo = stripComments e
      ,_reducesToWhenApplied = reduces
      }
  where
    ctx = natTypeCtx

    e :: CommentedExpr
    e =
      Lam natTypeName $                                       -- \n : Nat ->
        CaseAnalysis $ Case (Binding $ VZ)                    -- case n of
          $ CaseBranches                                      --
            (CaseBranch (sPat $ sPat Bind) (Binding VZ) :| [] --   S S n -> n
            )                                                 --
            (Just                                             --
                  zTerm                                       --   _     -> Z
            )
    ty = Arrow natType natType

    reduces =
      [ ("3 - 2 = 1"
        ,[three]
        ,one
        )

      , ( "2 -2 = 0"
        ,[two]
        ,zero
        )

      , ( "1 - 2 = 0"
        ,[one]
        ,zero
        )
      ]

