{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.Reducing.Type
  ( reducesTypesToSpec
  , reduceTypeToSpec
  )
  where

import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Kind
import PL.Reduce
import PL.Expr
import PL.ReduceType
import PL.TyVar
import PL.Type
import PL.Pattern
import PL.Name
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

import PL.Test.TypeTestCase

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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List
import Data.Text (Text)

import Test.Hspec
import PL.Test.Source
import PL.Test.Util


-- Test each testcase reduces to expected results.
reducesTypesToSpec
  :: Map.Map Text.Text TypeTestCase
  -> (ExprFor DefaultPhase -> Doc)
  -> (TypeFor DefaultPhase -> Doc)
  -> (PatternFor DefaultPhase -> Doc)
  -> Spec
reducesTypesToSpec testCases ppExpr ppType ppPattern =
  describe "All example types"
    . mapM_ (\(name,testCase) -> reduceTypeToSpec name (_isType testCase) (("Reduces", _underTypeCtx testCase, [], _reducesTo testCase) : _reducesToWhenApplied testCase) ppExpr ppType ppPattern)
    . Map.toList
    $ testCases

-- | Test whether a type reduces to another.
--
-- Name a type, apply it to a list of (argnames,argument,expected result) tuples.
-- Where the type in turn applied to each list of arguments must reduce to the given expected result
reduceTypeToSpec
  :: Text.Text
  -> TypeFor CommentedPhase
  -> [(Text.Text, TypeCtx DefaultPhase, [TypeFor DefaultPhase], TypeFor DefaultPhase)]
  -> (ExprFor DefaultPhase -> Doc)
  -> (TypeFor DefaultPhase -> Doc)
  -> (PatternFor DefaultPhase -> Doc)
  -> Spec
reduceTypeToSpec name inputType reductions ppExpr ppType ppPattern = describe (Text.unpack name) $
  mapM_ (\(name,underCtx,args,expectReduction) -> reduceSpec name underCtx (appise (stripTypeComments inputType : args)) expectReduction) reductions
  where
    reduceSpec
      :: Text.Text
      -> TypeCtx DefaultPhase
      -> Type
      -> Type
      -> Spec
    reduceSpec name underCtx typ eqType = it (Text.unpack name) $ case reduceType underCtx typ of
      Left typErr
        -> expectationFailure . Text.unpack . render . ppError ppPattern ppType ppExpr $ typErr

      Right redType
        -> if redType == eqType
             then return ()

             -- Doesnt equal initial type.
             -- reduce that type and check once more
             else case reduceType underCtx eqType of
                    Left eqTypeErr
                      -> expectationFailure . Text.unpack . render $ mconcat
                           [ text "target type reduces, doesnt match the expected type AND the expected type fails to reduce itself:"
                           , ppError ppPattern ppType ppExpr eqTypeErr
                           ]

                    Right redEqType
                      -> if redType == redEqType
                           then return ()
                           else expectationFailure "target and expected type both reduce BUT they are not equal"

    appise :: [Type] -> Type
    appise []        = error "Cant appise empty list of types"
    appise [t]       = t
    appise (t:t':ts) = appise (TypeApp t t' : ts)

