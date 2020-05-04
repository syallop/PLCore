{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.TypeLam
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'TypeLam' constructor.
-}
module PL.Test.Type.TypeLam
  ( typeLamTestCases
  , TestTypeLamSources (..)
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

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.TypeTestCase
import PL.Test.Source
import PL.Test.Shared

-- A record of the sources required to run all the TestTypeLamSources tests.
data TestTypeLamSources = TestTypeLamSources
  { _simpleTypeLamTestCase :: Source
  , _nestedTypeLamTestCase :: Source
  }

typeLamTestCases
  :: TestTypeLamSources
  -> [(Text,TypeTestCase)]
typeLamTestCases t =
  [ ("Simple Type Lam", simpleTypeLamTestCase . _simpleTypeLamTestCase $ t)
  , ("Nested Type Lam", nestedTypeLamTestCase . _nestedTypeLamTestCase $ t)
  ]

simpleTypeLamTestCase
  :: Source
  -> TypeTestCase
simpleTypeLamTestCase src
  = TypeTestCase
  {_underTypeCtx         = ctx
  ,_underTypeBindCtx     = emptyCtx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    ty  = TypeLam Kind $ TypeBinding $ TyVar VZ
    k = KindArrow Kind Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Bind types when applied"
          , _typeReductionUnderTypeCtx = ctx
          , _typeReductionUnderTypeBindings = emptyBindings
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`TypeApp` unitTypeName)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ unitTypeName
              ]
          }

      ]

nestedTypeLamTestCase
  :: Source
  -> TypeTestCase
nestedTypeLamTestCase src
  = TypeTestCase
  {_underTypeCtx         = ctx
  ,_underTypeBindCtx     = emptyCtx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    ty  = TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS VZ
    k = KindArrow Kind $ KindArrow Kind Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Reduces to outer type"
          , _typeReductionUnderTypeCtx = ctx
          , _typeReductionUnderTypeBindings = emptyBindings
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`TypeApp` boolTypeName)
              , (`TypeApp` natTypeName)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ boolTypeName
              ]
          }

      , TypeReductionTestCase
          { _typeReductionName = "Reduces under type lambda"
          , _typeReductionUnderTypeCtx = ctx
          , _typeReductionUnderTypeBindings = emptyBindings
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`TypeApp` boolTypeName)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeLam Kind $ boolTypeName
              ]
          }
      ]

