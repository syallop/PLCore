{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , RankNTypes
  #-}
{-|
Module      : PL.Test.ExprTestCase
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

Functions for testing expression parsing, typechecking, reduction, etc.
Also exports 'ExprTestCase' which encapsulates an example which can have all of these properties tested.
-}
module PL.Test.ExprTestCase
  ( ExprTestCase(..)
  , ReductionTestCase
  )
  where

import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Commented
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

-- ExprTestCase collects together common parameters for testcases on expressions
--
-- It's likely factored badly.
data ExprTestCase = ExprTestCase
  {_underTypeCtx :: TypeCtx DefaultPhase   -- ^ Under this given typing context
  ,_isExpr       :: ExprFor CommentedPhase -- ^ An Expr
  ,_typed        :: TypeFor DefaultPhase   -- ^ Has this type
  ,_parsesFrom   :: Text                   -- ^ And also parses from this textual representation

  ,_reducesTo :: ExprFor DefaultPhase      -- ^ Expr reduces to this form. E.G. when it contains lambdas applied to expressions.

  ,_reducesToWhenApplied :: [ReductionTestCase]
  }

-- A Reduction test has a name, a list of transformations and is expected to
-- fail or succeed with some reduced expression.
type ReductionTestCase = (Text, [ExprFor DefaultPhase -> ExprFor DefaultPhase], Maybe (ExprFor DefaultPhase))

