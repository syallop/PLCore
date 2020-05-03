{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.Product
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'Product' constructor.
-}
module PL.Test.Type.Product
  ( productTestCases
  , TestProductSources (..)
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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.TypeTestCase
import PL.Test.Source
import PL.Test.Shared

-- A record of the sources required to run all the TestProductSources tests.
data TestProductSources = TestProductSources
  { _emptyProductTestCase     :: Source
  , _singletonProductTestCase :: Source
  , _twoProductTestCase       :: Source
  , _duplicateProductTestCase :: Source
  }

productTestCases
  :: TestProductSources
  -> [(Text,TypeTestCase)]
productTestCases t =
  [ ("Empty product", emptyProductTestCase . _emptyProductTestCase $ t)
  , ("Singleton product", singletonProductTestCase . _singletonProductTestCase $ t)
  , ("Two product", twoProductTestCase . _twoProductTestCase $ t)
  , ("Duplicate product", duplicateProductTestCase . _duplicateProductTestCase $ t)
  ]

emptyProductTestCase
  :: Source
  -> TypeTestCase
emptyProductTestCase src
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
    ty  = ProductT []
    k = Kind

    reduces =
      [
      ]

singletonProductTestCase
  :: Source
  -> TypeTestCase
singletonProductTestCase src
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
    ty  = ProductT [unitTypeName]
    k = Kind

    reduces =
      [
      ]

twoProductTestCase
  :: Source
  -> TypeTestCase
twoProductTestCase src
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
    ty  = ProductT [unitTypeName,natTypeName]
    k = Kind

    reduces =
      [ ( "Is not the same as it's reverse"
        , ctx
        , []
        , [TypeDoesNotEqual $ ProductT [natTypeName, unitTypeName]]
        )
      ]

duplicateProductTestCase
  :: Source
  -> TypeTestCase
duplicateProductTestCase src
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
    ty  = ProductT [unitTypeName,unitTypeName]
    k = Kind

    reduces =
      [ ( "Does not lose duplicates"
        , ctx
        , []
        , [TypeDoesNotEqual $ ProductT [unitTypeName]]
        )
      ]

