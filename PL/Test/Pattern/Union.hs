{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Pattern.Union
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.Pattern.Union
  (TestUnionSources (..)
  , unionTestCases
  )
  where

import PL.Type
import PL.TypeCtx
import PL.TypeCheck
import PL.Pattern

import Data.Text (Text)

import qualified Data.Set as Set

import PL.Test.PatternTestCase

import PL.Test.Source

data TestUnionSources = TestUnionSources
  { _unionTestCase :: Source
  }

-- Test the union constructor of Patterns.
unionTestCases
  :: TestUnionSources
  -> [(Text, PatternTestCase)]
unionTestCases t =
  [("Single union", unionPatternTestCase . _unionTestCase $ t)
  ]

-- A simple Pattern on a union constructor.
-- Intended to be used in more complex test cases by field substitution.
defaultUnionPatternTestCase
  :: Source
  -> PatternTestCase
defaultUnionPatternTestCase src
  = PatternTestCase
      { _parsesFrom = src
      , _parsesTo   = UnionPattern EmptyProductT EmptyProductPattern

      , _underResolveCtx = undefined
      , _resolvesTo = UnionPattern EmptyProductT EmptyProductPattern

      , _underTypeCheckCtx = topTypeCheckCtx emptyTypeCtx
      , _typed             = UnionT $ Set.fromList $ [EmptyProductT]

      , _bindsOnMatch = Right []
      }
  where

unionPatternTestCase
  :: Source
  -> PatternTestCase
unionPatternTestCase
  = defaultUnionPatternTestCase

