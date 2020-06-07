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

import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Pattern
import PL.Type.Eq
import PL.TypeCtx
import PL.TypeCheck
import PL.FixPhase
import PL.Var
import PL.Resolve
import PL.Bindings

import PLGrammar
import PLPrinter

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))
import Data.List
import Data.Text (Text)
import qualified Data.Map as Map

import Test.Hspec
import PL.Test.Source
import PL.Test.Util

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
  ,_checkMatchWithResult :: Either Error [Type] -- ^ Either produces an error or a list of bound types.
  }

