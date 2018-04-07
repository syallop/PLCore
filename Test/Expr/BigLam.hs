{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Test.Expr.BigLam
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'BigLam' constructor.
-}
module Test.Expr.BigLam
  ( bigLamTypeCtx
  , bigLamTestCases
  , TestBigLamSources (..)
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.FixType
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))

import Test.ExprTestCase
import Test.Source

data TestBigLamSources = TestBigLamSources
  { _singleBigLamTestCase :: Source
  }

bigLamTypeCtx = emptyTypeCtx

bigLamTestCases
  :: TestBigLamSources
  -> [(Text,ExprTestCase)]
bigLamTestCases t =
  [("Single big-lambda" , singleBigLamTestCase . _singleBigLamTestCase $ t)
  ]

-- \(a::k) -> a
-- Test a single big-lambda that takes and returns a type.
-- (id)
singleBigLamTestCase
  :: Source
  -> ExprTestCase
singleBigLamTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src
      }
  where
    ctx = bigLamTypeCtx
    e   = fixExpr $ BigLam Kind
            (fixExpr $ Lam (fixType . TypeBinding . TyVar $ VZ)
                           (fixExpr $ Binding VZ)
            )

    -- forall k. (a :: k) -> (a :: k)
    ty  = fixType $ BigArrow
            Kind
            (fixType $ Arrow (fixType . TypeBinding . TyVar $ VZ)
                             (fixType . TypeBinding . TyVar $ VZ)
            )

