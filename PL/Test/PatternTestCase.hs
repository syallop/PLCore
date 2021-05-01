{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , FlexibleContexts
  #-}
{-|
Module      : PL.Test.PatternTestCase
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.Test.PatternTestCase where

import PL.Commented
import PL.Type
import PL.Pattern
import PL.TypeCheck
import PL.FixPhase
import PL.Resolve

import Data.Text (Text)

type TestType = Type
type TestPattern = Pattern

data PatternTestCase = PatternTestCase
  { -- Parsing tests
    _parsesFrom :: Text
  , _parsesTo   :: PatternFor CommentedPhase

    -- Resolution tests
  , _underResolveCtx :: ResolveCtx
  , _resolvesTo      :: Pattern

    -- Type checking tests
  , _underTypeCheckCtx    :: TypeCheckCtx
  , _typed                :: Type

    -- Matching tests
  ,_bindsOnMatch :: Either Error [Type] -- ^ Either produces an error or a list of bound types.
  }

