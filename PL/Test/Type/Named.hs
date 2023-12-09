{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
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

import PL.Binds
import PL.FixPhase
import PL.Kind
import PL.ReduceType
import PL.Type
import PL.TypeCheck
import PL.TypeCtx

import Data.Maybe
import Data.Text (Text)
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
  :: (NamedExtension    phase ~ NoExt
     ,SumTExtension     phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     )
  => TypeCtxFor phase
namedTypeCtx
  = fromJust
  . insertType "Preexisting" preExistingType Kind
  . fromJust
  . insertRecType "Recursive" recursiveType Kind
  $ emptyTypeCtx

preExistingType
  :: (SumTExtension     phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     )
  => TypeFor phase
preExistingType = SumT $ NE.fromList [EmptyProductT]

recursiveType
  :: (SumTExtension phase     ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,NamedExtension phase    ~ NoExt
     )
  => TypeFor phase
recursiveType = SumT $ NE.fromList [EmptyProductT,TypeSelfBinding]

simpleNameTestCase
  :: Source
  -> TypeTestCase
simpleNameTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo = Named "Preexisting"

  , _underResolveCtx = undefined
  , _resolvesTo = Named "Preexisting"

  , _underTypeCheckCtx = topTypeCheckCtx namedTypeCtx
  , _hasKind = Kind

  , _underTypeReductionCtx = topTypeReductionCtx namedTypeCtx
  , _reducesTo = Named "Preexisting"
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Named types reduce to their definition"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx namedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ SumT $ NE.fromList [EmptyProductT]
              ]
          }
      ]
  }

recursiveNameTestCase
  :: Source
  -> TypeTestCase
recursiveNameTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo = Named "Recursive"

  , _underResolveCtx = undefined
  , _resolvesTo = Named "Recursive"

  , _underTypeCheckCtx = topTypeCheckCtx namedTypeCtx
  , _hasKind = Kind

  , _underTypeReductionCtx = topTypeReductionCtx namedTypeCtx
  , _reducesTo = Named "Recursive"
  , _reducesToWhenApplied =
    [ TypeReductionTestCase
          { _typeReductionName = "Equal themselves"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx namedTypeCtx
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
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx namedTypeCtx
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
  }

