{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec.BigLam
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'BigLam' constructor.
-}
module ExprSpec.BigLam
  ( bigLamTypeCtx
  , bigLamTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Kind
import PL.Parser
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

import ExprTestCase

bigLamTypeCtx = emptyTypeCtx

bigLamTestCases :: [(Text,ExprTestCase)]
bigLamTestCases =
  [("Single big-lambda" , singleBigLamTestCase)
  ]

-- \(a::k) -> a
-- Test a single big-lambda that takes and returns a type.
-- (id)
singleBigLamTestCase :: ExprTestCase
singleBigLamTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
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
    txt = "Λ KIND λ?0 0"

