{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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
import PL.Commented
import PL.Error
import PL.Expr
import PL.FixPhase
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

-- A record of the sources required to run all the TestNamedSources tests.
data TestNamedSources = TestNamedSources
  { _simpleNameTestCase    :: Source
  , _recursiveNameTestCase :: Source
  }

namedTestCases
  :: TestNamedSources
  -> [(Text,TypeTestCase)]
namedTestCases t =
  [ ("Simple name"   , simpleNameTestCase    . _simpleNameTestCase    $ t)
  , ("Recursive name", recursiveNameTestCase . _recursiveNameTestCase $ t)
  ]

namedTypeCtx
  :: (NamedExtension    phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => TypeCtxFor phase
namedTypeCtx
  = fromJust
  . insertType "Preexisting" preExistingType Kind
  . fromJust
  . insertRecType "Recursive" recursiveType Kind
  $ emptyTypeCtx

preExistingType
  :: (SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => TypeFor phase
preExistingType = SumT $ NE.fromList [EmptyProductT]

recursiveType
  :: (SumTExtension phase     ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension phase    ~ Void
     )
  => TypeFor phase
recursiveType = SumT $ NE.fromList [EmptyProductT,Named "Recursive"]

simpleNameTestCase
  :: Source
  -> TypeTestCase
simpleNameTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx ctx
  ,_underTypeBindCtx      = emptyCtx
  ,_isType                = ty
  ,_parsesFrom            = src
  ,_hasKind               = k
  ,_reducesTo             = stripTypeComments ty
  ,_reducesToWhenApplied  = reduces
  }
  where
    ctx = namedTypeCtx
    ty  = Named "Preexisting"
    k = Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Named types reduce to their definition"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ SumT $ NE.fromList [EmptyProductT]
              ]
          }
      ]

recursiveNameTestCase
  :: Source
  -> TypeTestCase
recursiveNameTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx ctx
  ,_isType                = ty
  ,_parsesFrom            = src
  ,_hasKind               = k
  ,_reducesTo             = stripTypeComments ty
  ,_reducesToWhenApplied  = reduces
  }
  where
    ctx = namedTypeCtx
    ty  = Named "Recursive"
    k = Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Equal themselves"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ Named "Recursive"
              ]
          }
      , TypeReductionTestCase
          { _typeReductionName = "Equal unwrappings of themselves"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [TypeEquals $ recursiveType
              ,TypeEquals $ SumT $ NE.fromList [EmptyProductT,SumT $ NE.fromList [EmptyProductT,Named "Recursive"]]
              ]
          }

      ]


