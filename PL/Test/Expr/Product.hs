{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.Product
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'product' type.
-}
module PL.Test.Expr.Product
  ( productThreeExprTestCase

  , TestProductSources (..)
  , productTestCases
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
import PL.Pattern

import Data.Text (Text)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))

import PL.Test.Expr.Natural
import PL.Test.Expr.Boolean

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

data TestProductSources = TestProductSources
  { _productThreeTestCase :: Source
  }

productTestCases
  :: TestProductSources
  -> [(Text, ExprTestCase)]
productTestCases t =
  [ ("product three types", productThreeExprTestCase . _productThreeTestCase $ t)
  ]

productThreeExprTestCase
  :: Source
  -> ExprTestCase
productThreeExprTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam (ProductT [natTypeName,boolTypeName,natTypeName]) $ -- \x : Nat*Bool*Nat ->
          CaseAnalysis $ Case (Binding VZ)                                      -- case x of
            $ CaseBranches                                                                --
              (CaseBranch (ProductPattern [zPat,Bind,zPat]) (Binding VZ)          -- Z,y,Z -> y
               :| [CaseBranch (ProductPattern [Bind,Bind,zPat]) (Binding VZ)]     -- x,y,Z -> y
              )                                                                 --
              (Just                                                             --
                  falseTerm                                                     -- _ -> False
              )

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam (ProductT [natTypeName,boolTypeName,natTypeName]) $ -- \x : Nat*Bool*Nat ->
          CaseAnalysis $ Case (Binding VZ)                                      -- case x of
            $ CaseBranches                                                                --
              (CaseBranch (ProductPattern [zPat,Bind,zPat]) (Binding VZ)          -- Z,y,Z -> y
               :| [CaseBranch (ProductPattern [Bind,Bind,zPat]) (Binding VZ)]     -- x,y,Z -> y
              )                                                                 --
              (Just                                                             --
                  falseTerm                                                     -- _ -> False
              )

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed             = Arrow (ProductT [natTypeName,boolTypeName,natTypeName]) boolTypeName

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam (ProductT [natTypeName,boolTypeName,natTypeName]) $ -- \x : Nat*Bool*Nat ->
          CaseAnalysis $ Case (Binding VZ)                                      -- case x of
            $ CaseBranches                                                                --
              (CaseBranch (ProductPattern [zPat,Bind,zPat]) (Binding VZ)          -- Z,y,Z -> y
               :| [CaseBranch (ProductPattern [Bind,Bind,zPat]) (Binding VZ)]     -- x,y,Z -> y
              )                                                                 --
              (Just                                                             --
                  falseTerm                                                     -- _ -> False
              )
      , _reducesToWhenApplied =
            [("1 True 0"
             ,[(`App` Product [one,trueTerm,zero])]
             ,Just trueTerm
             )

            ,("1 False 0"
             , [(`App` Product [one,falseTerm,zero])]
             , Just falseTerm
             )

            , ("1 True 1"
              ,[(`App` Product [one,trueTerm,one])]
              ,Just falseTerm
              )

            , ("4 False 1"
              ,[(`App` Product [four,falseTerm,one])]
              ,Just falseTerm
              )
            ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = natTypeCtx <> boolTypeCtx

