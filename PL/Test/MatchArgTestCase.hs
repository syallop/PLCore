{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , FlexibleContexts
  #-}
{-|
Module      : PL.Test.MatchArgTestCase
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.Test.MatchArgTestCase where

import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.MatchArg
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
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
type TestMatchArg = MatchArg

data MatchArgTestCase = MatchArgTestCase
  {_underTypeCtx         :: TypeCtx DefaultPhase                     -- ^ Under this given typing context
  ,_underExprBindCtx     :: BindCtx Var (TypeFor DefaultPhase)
  ,_underTypeBindCtx     :: BindCtx TyVar Kind
  ,_underTypeBindings    :: Bindings (TypeFor DefaultPhase)
  ,_isMatchArg           :: MatchArgFor CommentedPhase -- ^ A MatchArg
  ,_typed                :: Type                        -- ^ Has this type
  ,_checkMatchWithResult :: Either (Error Type MatchArg) [TypeFor DefaultPhase] -- ^ Either produces an error or a list of bound types.
  ,_parsesFrom           :: Text                              -- ^ And also parses from this textual representation
  }

