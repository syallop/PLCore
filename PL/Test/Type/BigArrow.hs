{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.BigArrow
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'BigArrow' constructor.
-}
module PL.Test.Type.BigArrow
  ( bigArrowTestCases
  , TestBigArrowSources (..)
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

-- A record of the sources required to run all the TestBigArrowSources tests.
data TestBigArrowSources = TestBigArrowSources
  { _simpleBigArrowTestCase :: Source
  }

bigArrowTestCases
  :: TestBigArrowSources
  -> [(Text,TypeTestCase)]
bigArrowTestCases t =
  [ ("Simple", simpleBigArrowTestCase . _simpleBigArrowTestCase $ t)
  ]

simpleBigArrowTestCase
  :: Source
  -> TypeTestCase
simpleBigArrowTestCase src
  = TypeTestCase
  {_underTypeCtx         = ctx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    ty  = BigArrow Kind unitTypeName
    k = Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Can be nested in the second argument"
          , _typeReductionUnderTypeCtx = ctx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionUnderTypeBindings = emptyBindings
          , _typeReductionMutateType =
              [ (Kind `BigArrow`)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ BigArrow Kind (BigArrow Kind unitTypeName)
              ]
          }
      ]

