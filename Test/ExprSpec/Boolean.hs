{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec.Boolean
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a 'Boolean' type.
-}
module ExprSpec.Boolean
  ( boolTypeCtx
  , boolTypeName
  , boolType
  , boolSumType
  , falseTerm
  , trueTerm
  , falsePat
  , truePat
  , falseTermText
  , trueTermText
  , falsePatText
  , truePatText

  , andExprTestCase
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.FixType
import PL.Var

import PLParser

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))

import ExprTestCase

boolTypeCtx = insertType "Bool" boolType emptyTypeCtx
boolTypeName = fixType $ Named "Bool"
boolType = fixType $ SumT boolSumType
boolSumType = map fixType
                [ProductT []
                ,ProductT []
                ]
falseTerm     = fixExpr $ Sum (fixExpr $ Product []) 0 boolSumType
trueTerm      = fixExpr $ Sum (fixExpr $ Product []) 1 boolSumType
falsePat      = MatchSum 0 (MatchProduct [])
truePat       = MatchSum 1 (MatchProduct [])

falseTermText, trueTermText, falsePatText, truePatText :: Text

falseTermText = "+0 (*) (*) (*)"
trueTermText  = "+1 (*) (*) (*)"
falsePatText  = "(+0 (*))"
truePatText   = "(+1 (*))"

-- Boolean and
andExprTestCase :: ExprTestCase
andExprTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = fromJust boolTypeCtx
    e   = fixExpr $ Lam boolTypeName $ fixExpr $ Lam boolTypeName $ fixExpr $ -- \x:Bool y:Bool ->
        CaseAnalysis $ Case (fixExpr $ Binding VZ)                            -- case y of
          $ CaseBranches                                                      --
            (CaseBranch falsePat falseTerm :| []                              --     False -> False
            )                                                                 --
            (Just                                                             --     _      ->
                (fixExpr $ CaseAnalysis $ Case (fixExpr $ Binding $ VS VZ)    --               case x of
                  $ CaseBranches                                              --
                    (CaseBranch falsePat falseTerm :|[]                       --                 False -> False
                    )                                                         --
                    (Just                                                     --                        _     ->
                        trueTerm                                              --                                 True
                    )
                )
            )
    ty  = fixType $ Arrow boolType (fixType $ Arrow boolType boolType)
    txt = Text.unlines
      ["λBool λBool (CASE 0"
      ,"               (|"<>falsePatText<>" "<>falseTermText<>")"
      ,""
      ,"               (CASE 1"
      ,"                   (|"<>falsePatText<>" "<>falseTermText<>")"
      ,""
      ,"                   "<>trueTermText<>""
      ,""
      ,"               )"
      ,"            )"
      ]

