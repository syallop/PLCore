{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Pattern.SelfTypes
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Pattern using self types.
-}
module PL.Test.Pattern.SelfType
  ( TestSelfTypeSources (..)
  , selfTypeTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Pattern
import PL.Reduce
import PL.TyVar
import PL.TypeCtx
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.TypeCheck
import PL.Var

import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE

import PL.Test.PatternTestCase

import PL.Test.Shared
import PL.Test.Source

data TestSelfTypeSources = TestSelfTypeSources
  { _emptyProductSource      :: Source
  , _bindProductSource       :: Source
  , _nestedBindProductSource :: Source
  }

selfTypeTestCases
  :: TestSelfTypeSources
  -> [(Text, PatternTestCase)]
selfTypeTestCases t =
  [ ("Mu types are destructed before matching"
    , PatternTestCase
      { _parsesFrom = _emptyProductSource t
      , _parsesTo   = EmptyProductPattern

      , _underResolveCtx = undefined
      , _resolvesTo      = EmptyProductPattern

      , _underTypeCheckCtx = topTypeCheckCtx emptyTypeCtx
      , _typed             = TypeMu Kind $ EmptyProductT -- Note no self variables.

      , _bindsOnMatch = Right []
      }
    )

  , ("Matching self variables means the pattern matches the self definition"
    , PatternTestCase
      { _parsesFrom = _bindProductSource t
      , _parsesTo   = ProductPattern [Bind]

      , _underResolveCtx = undefined
      , _resolvesTo      = ProductPattern [Bind]

      , _underTypeCheckCtx = topTypeCheckCtx emptyTypeCtx
      , _typed             = TypeMu Kind $ ProductT [TypeSelfBinding]

        -- Note the pattern does not claim to bind a TypeSelfBinding and must
        -- unroll the definition.
      , _bindsOnMatch = Right [TypeMu Kind $ ProductT [TypeSelfBinding]]
      }
    )

  , ( "Nested self variables match"
    , PatternTestCase
      { _parsesFrom = _nestedBindProductSource t
      , _parsesTo   = ProductPattern [ProductPattern [Bind]]

      , _underResolveCtx = undefined
      , _resolvesTo      = ProductPattern [ProductPattern [Bind]]

      , _underTypeCheckCtx = topTypeCheckCtx emptyTypeCtx
      , _typed             = TypeMu Kind $ ProductT [TypeSelfBinding]

        -- Note the pattern does not claim to bind a TypeSelfBinding and must
        -- unroll the definition.
      , _bindsOnMatch = Right [TypeMu Kind $ ProductT [TypeSelfBinding]]
      }
    )
  ]

