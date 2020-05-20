{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.Arrow
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'Arrow' constructor.
-}
module PL.Test.Type.Arrow
  ( arrowTestCases
  , TestArrowSources (..)
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
import PL.ReduceType
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

-- A record of the sources required to run all the TestArrowSources tests.
data TestArrowSources = TestArrowSources
  { _simpleArrowTestCase :: Source
  }

arrowTestCases
  :: TestArrowSources
  -> [(Text,TypeTestCase)]
arrowTestCases t =
  [ ("Simple Arrow", simpleArrowTestCase . _simpleArrowTestCase $ t)
  ]

simpleArrowTestCase
  :: Source
  -> TypeTestCase
simpleArrowTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx ctx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    ty  = Arrow unitTypeName unitTypeName
    k = Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Can be nested in the first argument"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`Arrow` boolTypeName)
              ]
          , _typeReductionMatches =
              [TypeEquals $ Arrow (Arrow unitTypeName unitTypeName) boolTypeName
              ,TypeDoesNotEqual $ Arrow unitTypeName (Arrow unitTypeName boolTypeName)
              ]
          }

      , TypeReductionTestCase
          { _typeReductionName = "Can be nested in the second argument"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (boolTypeName `Arrow`)
              ]
          , _typeReductionMatches =
              [TypeEquals $ Arrow boolTypeName (Arrow unitTypeName unitTypeName)
              ,TypeDoesNotEqual $ Arrow (Arrow boolTypeName unitTypeName) unitTypeName
              ]
          }
      ]

