{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec.Natural
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a 'Natural' type.
-}
module ExprSpec.Natural
  ( natTypeCtx
  , natTypeName
  , natType
  , natSumType
  , zTerm
  , sTerm
  , zPat
  , sPat
  , zTermText
  , sTermText
  , zPatText

  , zero
  , succ
  , one
  , two
  , three
  , four

  , subTwoExprTestCase
  )
  where

import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Parser
import PL.Parser.Lispy hiding (appise,lamise)
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

import Data.Text (Text)
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text

import Data.Monoid ((<>))

import ExprTestCase

natTypeCtx = insertRecType "Nat" natType emptyTypeCtx
natTypeName = Named "Nat"
natType    = SumT natSumType
natSumType = [ProductT []
             ,Named "Nat"
             ]
zTerm      =                     Sum (Product [])        0 natSumType
sTerm      = Lam (Named "Nat") $ Sum (Binding (mkVar 0)) 1 natSumType
zPat       = MatchSum 0 (MatchProduct [])
sPat p     = MatchSum 1 p

zTermText, sTermText, zPatText :: Text
zTermText  = "+0(*) (*) Nat"
sTermText  = "\\Nat (+1 0 (*) Nat)"
zPatText   = "+0(*)"
sPatText p = "+1"<>p

zero  = zTerm
suc n = sTerm `App` n
one   = suc zero
two   = suc one
three = suc two
four  = suc three

-- Test nested pattern matching
-- n > 2     ~> n-2
-- otherwise ~> 0
subTwoExprTestCase :: ExprTestCase
subTwoExprTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = fromJust natTypeCtx
    e   = Lam natTypeName $                                         -- \n : Nat ->
            CaseAnalysis $ Case (Binding VZ)                        -- case n of
              $ CaseBranches                                        --
                ((CaseBranch (sPat $ sPat Bind) (Binding VZ)) :| [] --   S S n -> n
                )                                                   --
                (Just                                               --
                    zTerm                                           --   _     -> Z
                )
    ty = Arrow natType natType

    txt = Text.unlines
      ["\\Nat (CASE 0"
      ,"         (|"<>(sPatText $ sPatText "?")<>" 0)"
      ,"         "<>zTermText
      ,"     )"
      ]

