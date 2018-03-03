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

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.FixExpr
import PL.Expr
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

import Data.Text (Text)
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text

import Data.Monoid ((<>))

import ExprTestCase

natTypeCtx = insertRecType "Nat" natType emptyTypeCtx
natTypeName = fixType $ Named "Nat"
natType    = fixType $ SumT natSumType
natSumType = map fixType $
               [ProductT []
               ,Named "Nat"
               ]

zTerm, sTerm :: Expr Var (Type tb) tb
zTerm      = fixExpr $                                         Sum (fixExpr $ Product [])        0 natSumType
sTerm      = fixExpr $ Lam (fixType $ Named "Nat") $ fixExpr $ Sum (fixExpr $ Binding (mkVar 0)) 1 natSumType

zPat :: MatchArg Var tb
zPat = MatchSum 0 (MatchProduct [])

sPat :: MatchArg Var tb -> MatchArg Var tb
sPat = MatchSum 1

zTermText, sTermText, zPatText :: Text
zTermText  = "+0(*) (*) Nat"
sTermText  = "λNat (+1 0 (*) Nat)"
zPatText   = "+0(*)"
sPatText p = "+1"<>p

suc :: Expr Var (Type tb) tb -> Expr Var (Type tb) tb
suc n = fixExpr $ App sTerm (n)

zero, one, two, three, four :: Expr Var (Type tb) tb
zero  = zTerm
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

    e :: Expr Var (Type tb) tb
    e = fixExpr $
      Lam natTypeName $ fixExpr $                                       -- \n : Nat ->
        CaseAnalysis $ Case (fixExpr . Binding $ VZ)                    -- case n of
          $ CaseBranches                                                --
            (CaseBranch (sPat $ sPat Bind) (fixExpr $ Binding VZ) :| [] --   S S n -> n
            )                                                           --
            (Just                                                       --
                  zTerm                                                 --   _     -> Z
            )
    ty = fixType $ Arrow natType natType

    txt = Text.unlines
      ["λNat (CASE 0"
      ,"         (|" <> sPatText (sPatText "?") <>" 0)"
      ,"         "<>zTermText
      ,"     )"
      ]

