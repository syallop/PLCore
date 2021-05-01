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

import PL.Binds
import PL.Commented
import PL.Kind
import PL.FixPhase
import PL.ReduceType
import PL.TyVar
import PL.Resolve
import PL.Type
import PL.TypeCheck

import Data.Text (Text)

-- TypeTestCase collects together common parameters for testcases on types
--
-- It's likely factored badly.
data TypeTestCase = TypeTestCase
  { -- Parsing tests
    _parsesFrom            :: Text    -- ^ Parses from this textual representation
  , _parsesTo              :: TypeFor CommentedPhase

    -- Resolution tests
  , _underResolveCtx       :: ResolveCtx
  , _resolvesTo            :: Type

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

