{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.BigLam
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'BigLam' constructor.
-}
module PL.Test.Expr.BigLam
  ( bigLamTestCases
  , TestBigLamSources (..)
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

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

data TestBigLamSources = TestBigLamSources
  { _singleBigLamTestCase :: Source
  }

bigLamTestCases
  :: TestBigLamSources
  -> [(Text,ExprTestCase)]
bigLamTestCases t =
  [("Single big-lambda" , singleBigLamTestCase . _singleBigLamTestCase $ t)
  ]

-- Test a single big-lambda that takes and returns a type.
-- (id)
--
-- /\a. \(x:a). x
singleBigLamTestCase
  :: Source
  -> ExprTestCase
singleBigLamTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = BigLam Kind
            (Lam (TypeBinding . TyVar $ VZ)
                 (Binding VZ)
            )

      , _underResolveCtx = undefined
      , _resolvesTo = BigLam Kind
            (Lam (TypeBinding . TyVar $ VZ)
                 (Binding VZ)
            )

      , _underTypeCheckCtx = topTypeCheckCtx ctx
        -- forall (a :: Kind). a -> a
      , _typed = BigArrow
            Kind
            (Arrow (TypeBinding . TyVar $ VZ)
                   (TypeBinding . TyVar $ VZ)
            )

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = BigLam Kind
            (Lam (TypeBinding . TyVar $ VZ)
                 (Binding VZ)
            )
      , _reducesToWhenApplied =
          [ ("Can be applied to types"
            , [(`BigApp` boolTypeName)
              ]
            , Just $ Lam boolTypeName $ Binding $ VZ
            )
          , ("Can be used to specify types for lambdas"
            , [(`BigApp` boolTypeName)
              ,(`App` trueTerm)
              ]
            , Just $ trueTerm
            )

          , ("Cannot be applied to values"
            , [(`App` trueTerm)
              ]
            , Nothing
            )
          ]

      , _underEvaluationCtx     = undefined
      , _evaluatesTo            = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = boolTypeCtx

