{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.BigLam
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'BigLam' constructor.
-}
module PL.Test.Expr.BigLam
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

import PL.Test.ExprTestCase
import PL.Test.Source

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

      ,_reducesTo = e
      ,_reducesToWhenApplied = []
      }
  where
    ctx = bigLamTypeCtx
    e   = BigLam Kind
            (Lam (TypeBinding . TyVar $ VZ)
                           (Binding VZ)
            )

    -- forall k. (a :: k) -> (a :: k)
    ty  = BigArrow
            Kind
            (Arrow (TypeBinding . TyVar $ VZ)
                             (TypeBinding . TyVar $ VZ)
            )

