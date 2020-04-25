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
  )
  where

import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Name
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings


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

data TypeTestCase = TypeTestCase
  {_underTypeCtx          :: TypeCtx DefaultPhase -- ^ Under this given typing context
  ,_underTypeBindCtx      :: BindCtx (TypeBindingFor DefaultPhase) Kind
  ,_isType                :: TypeFor DefaultPhase -- ^ An Expr
  ,_parsesFrom            :: Text                 -- ^ And also parses from this textual representation
  ,_hasKind               :: Kind
  , _reducesTo            :: TypeFor DefaultPhase -- ^ Type reduces to this form. E.G. when it contains type lambdas applied to types.
  , _reducesToWhenApplied :: [(Text,TypeCtx DefaultPhase, [TypeFor DefaultPhase], TypeFor DefaultPhase)] -- ^ When type-applied to a list of arguments, reduces to some result
  }
