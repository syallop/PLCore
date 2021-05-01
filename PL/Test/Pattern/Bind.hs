{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Pattern.Bind
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.Pattern.Bind
  ( TestBindSources (..)
  , bindTestCases
  )
  where

import PL.Pattern
import PL.TypeCtx
import PL.Type
import PL.TypeCheck

import Data.Text (Text)
import qualified Data.List.NonEmpty as NE

import PL.Test.PatternTestCase

import PL.Test.Shared
import PL.Test.Source

data TestBindSources = TestBindSources
  { _bindEmptySum     :: Source
  , _bindEmptyProduct :: Source
  , _bindNamedBoolean :: Source
  }

bindTestCases
  :: TestBindSources
  -> [(Text, PatternTestCase)]
bindTestCases t =
  [("Bind empty sum"    , bindSumPatternTestCase  . _bindEmptySum     $ t)
  ,("Bind empty product", bindProdPatternTestCase . _bindEmptyProduct $ t)
  ,("Bind named boolean", bindBoolPatternTestCase . _bindNamedBoolean $ t)
  ]

defaultBindPatternTestCase
  :: Source
  -> PatternTestCase
defaultBindPatternTestCase src
  = PatternTestCase
      {_underTypeCheckCtx = topTypeCheckCtx emptyTypeCtx
      ,_typed             = typed
      ,_bindsOnMatch      = checkMatchWithResult
      ,_parsesFrom        = parsesFrom
      ,_parsesTo          = Bind

      ,_underResolveCtx = undefined
      ,_resolvesTo      = Bind
      }
  where
    typed                = SumT $ NE.fromList [EmptyProductT]
    checkMatchWithResult = Right [SumT $ NE.fromList [EmptyProductT]]
    parsesFrom           = src

-- Test binding an empty sum.
bindSumPatternTestCase
  :: Source
  -> PatternTestCase
bindSumPatternTestCase = defaultBindPatternTestCase

-- Test binding an empty product.
bindProdPatternTestCase
  :: Source
  -> PatternTestCase
bindProdPatternTestCase src = (defaultBindPatternTestCase src)
  { _typed = EmptyProductT
  , _bindsOnMatch = Right [EmptyProductT]
  }

bindBoolPatternTestCase
  :: Source
  -> PatternTestCase
bindBoolPatternTestCase src = (defaultBindPatternTestCase src)
  { _underTypeCheckCtx = topTypeCheckCtx boolTypeCtx
  , _typed             = boolTypeName
  , _bindsOnMatch      = Right [boolType]
  }

