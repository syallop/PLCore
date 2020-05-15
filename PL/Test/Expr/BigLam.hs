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

-- \(a::k) -> a
-- Test a single big-lambda that takes and returns a type.
-- (id)
singleBigLamTestCase
  :: Source
  -> ExprTestCase
singleBigLamTestCase src
  = ExprTestCase
      { _underTypeCheckCtx = topTypeCheckCtx ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      ,_reducesTo = stripComments e
      ,_reducesToWhenApplied = reduces
      }
  where
    ctx = boolTypeCtx

    -- /\a. \(x:a). x
    e   = BigLam Kind
            (Lam (TypeBinding . TyVar $ VZ)
                 (Binding VZ)
            )

    -- forall (a :: Kind). a -> a
    ty  = BigArrow
            Kind
            (Arrow (TypeBinding . TyVar $ VZ)
                   (TypeBinding . TyVar $ VZ)
            )

    reduces =
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
