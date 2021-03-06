{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Pattern.Sum
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.Pattern.Sum
  ( TestSumSources (..)
  , sumTestCases
  )
  where

import PL.Type
import PL.TypeCtx
import PL.TypeCheck
import PL.Pattern

import Data.Text (Text)
import qualified Data.List.NonEmpty as NE

import PL.Test.PatternTestCase

import PL.Test.Source

data TestSumSources = TestSumSources
  { _sumTestCase :: Source
  }

-- Test the sum constructor of Patterns.
sumTestCases
  :: TestSumSources
  -> [(Text,PatternTestCase)]
sumTestCases t =
  [("Empty sum", sumPatternTestCase . _sumTestCase $ t)
  ]

-- (One of) the simplest Patterns on a sum constructor. Intended to be used in
-- more complex test cases by field substitution.
defaultSumPatternTestCase
  :: Source
  -> PatternTestCase
defaultSumPatternTestCase src
  = PatternTestCase
      { _parsesFrom = src
      , _parsesTo   = SumPattern 0 EmptyProductPattern

      , _underResolveCtx = undefined
      , _resolvesTo      = SumPattern 0 EmptyProductPattern

      , _underTypeCheckCtx = topTypeCheckCtx typeCtx
      , _typed             = SumT $ NE.fromList [EmptyProductT]

      , _bindsOnMatch = Right []
      }
  where
    typeCtx = emptyTypeCtx

sumPatternTestCase
  :: Source
  -> PatternTestCase
sumPatternTestCase
  = defaultSumPatternTestCase

