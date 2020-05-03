{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.Union
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'Union' constructor.
-}
module PL.Test.Type.Union
  ( unionTestCases
  , TestUnionSources (..)
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.TypeTestCase
import PL.Test.Source
import PL.Test.Shared

-- A record of the sources required to run all the TestUnionSources tests.
data TestUnionSources = TestUnionSources
  { _unionTwoTestCase :: Source
  , _singletonUnionTestCase :: Source
  , _duplicateUnionTestCase :: Source
  }

unionTestCases
  :: TestUnionSources
  -> [(Text,TypeTestCase)]
unionTestCases t =
  [ ("Union of two", unionTwoTestCase . _unionTwoTestCase $ t)
  , ("Singleton union", singletonUnionTestCase . _singletonUnionTestCase $ t)
  , ("Disallow multiple occurances of a type", duplicateUnionTestCase . _duplicateUnionTestCase $ t)
  ]

unionTwoTestCase
  :: Source
  -> TypeTestCase
unionTwoTestCase src
  = TypeTestCase
  {_underTypeCtx         = ctx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    ty  = UnionT $ Set.fromList [unitTypeName,natTypeName]
    k = Kind

    reduces =
      [ ( "Is the same as the reverse union"
        , ctx
        , []
        , [TypeEquals $ UnionT $ Set.fromList [natTypeName,unitTypeName]]
        )
      ]

singletonUnionTestCase
  :: Source
  -> TypeTestCase
singletonUnionTestCase src
  = TypeTestCase
  {_underTypeCtx         = ctx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    ty  = UnionT $ Set.fromList [unitTypeName]
    k = Kind

    reduces =
      [ ( "union(Unit) == union(Unit,Unit)"
        , ctx
        , []
        , [TypeEquals $ UnionT $ Set.fromList [unitTypeName,unitTypeName]]
        )
      ]

duplicateUnionTestCase
  :: Source
  -> TypeTestCase
duplicateUnionTestCase src
  = TypeTestCase
  {_underTypeCtx         = ctx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    ty  = UnionT $ Set.fromList [unitTypeName,unitTypeName]
    k = Kind

    -- TODO:
    -- - Should attempting this be an error?
    -- - Does using the Set type mean this test can't cover parsing and
    -- reduction?
    reduces =
      [ ( "Union cannot contain the same type multiple times"
        , ctx
        , []
        , [TypeEquals $ UnionT $ Set.fromList [unitTypeName]]
        )
      ]

