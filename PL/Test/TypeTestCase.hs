{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , RankNTypes
  #-}
{-|
Module      : PL.Test.TypeTestCase
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Functions for testing expression parsing, typechecking, reduction, etc.
Also exports 'TypeTestCase' which encapsulates an example which can have all of these properties tested.
-}
module PL.Test.TypeTestCase
  ( TypeTestCase(..)
  , TypeReductionTestCase(..)
  , TypeMatch(..)
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.Name
import PL.Reduce
import PL.ReduceType
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCheck
import PL.TypeCtx
import PL.Var

import PLGrammar
import PLPrinter
import PLPrinter.Doc

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List
import Data.Text (Text)

import Test.Hspec
import PL.Test.Source
import PL.Test.Util

-- TypeTestCase collects together common parameters for testcases on types
--
-- It's likely factored badly.
data TypeTestCase = TypeTestCase
  { _isType :: TypeFor CommentedPhase -- ^ A Type

   -- Parsing tests
  , _parsesFrom            :: Text    -- ^ Parses from this textual representation

  -- Type/ kind checking tests
  , _underTypeCheckCtx     :: TypeCheckCtx -- ^ Under this given typing context
  , _hasKind               :: Kind         -- ^ Has this kind

  -- Reduction tests
  , _underTypeReductionCtx :: TypeReductionCtx DefaultPhase
  , _reducesTo             :: Type                    -- ^ Type reduces to this form. E.G. when it contains type lambdas applied to types.
  , _reducesToWhenApplied  :: [TypeReductionTestCase] -- ^ When type-applied to a list of arguments, reduces to some result

  -- TODO: Evaluation tests?
  }

data TypeReductionTestCase = TypeReductionTestCase
  { _typeReductionName                  :: Text
  , _typeReductionUnderTypeReductionCtx :: TypeReductionCtx DefaultPhase
  , _typeReductionUnderTypeBindCtx      :: BindCtx TyVar Kind
  , _typeReductionMutateType            :: [Type -> Type]
  , _typeReductionMatches               :: [TypeMatch]
  }

data TypeMatch
  = TypeError
  | TypeEquals Type
  | TypeDoesNotEqual Type

