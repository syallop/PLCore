{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Type.Named
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'Named' constructor.
-}
module PL.Test.Type.Named
  ( namedTypeCtx
  , namedTestCases
  , TestNamedSources (..)
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

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.TypeTestCase
import PL.Test.Source

-- A record of the sources required to run all the TestNamedSources tests.
data TestNamedSources = TestNamedSources
  { _simpleNameTestCase :: Source
  }

namedTestCases
  :: TestNamedSources
  -> [(Text,TypeTestCase)]
namedTestCases t =
  [ ("Simple name" , simpleNameTestCase . _simpleNameTestCase $ t)
  ]

namedTypeCtx
  = fromJust
  . insertType "Preexisting" preExistingType
  $ emptyTypeCtx
preExistingType = SumT $ NE.fromList [ProductT []]

simpleNameTestCase
  :: Source
  -> TypeTestCase
simpleNameTestCase src
  = TypeTestCase
  {_underTypeCtx         = ctx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = namedTypeCtx
    ty  = Named "Preexisting"
    k = Kind

    -- TODO
    reduces = []
