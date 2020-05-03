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
    . mapM_ (\(name,testCase)
              -> reduceTypeToSpec name
                                  (_isType testCase)
                                  (("Reduces", _underTypeCtx testCase, [], [TypeEquals $ _reducesTo testCase]) : _reducesToWhenApplied testCase)
                                  ppExpr
                                  ppType
                                  ppPattern
            )
    . Map.toList
    $ testCases

-- | Test whether a type reduces to another.
--
-- Name a type, apply it to a list of (argnames,argument,expected result) tuples.
-- Where the type in turn applied to each list of arguments must reduce to the given expected result
reduceTypeToSpec
  :: Text.Text
  -> TypeFor CommentedPhase
  -> [TypeReductionTestCase]
  -> (ExprFor DefaultPhase -> Doc)
  -> (TypeFor DefaultPhase -> Doc)
  -> (PatternFor DefaultPhase -> Doc)
  -> Spec
reduceTypeToSpec name inputType reductions ppExpr ppType ppPattern = describe (Text.unpack name) $
  mapM_ (\(name,underCtx,args,expectReduction)
          -> let mutatedType = apply (stripTypeComments inputType) args
              in reduceSpec name underCtx mutatedType expectReduction) reductions
  where
    reduceSpec
      :: Text.Text
      -> TypeCtx DefaultPhase
      -> Type
      -> [TypeMatch]
      -> Spec
    reduceSpec name underCtx typ typeMatches = it (Text.unpack name) $
      let reducedType = reduceType underCtx typ
       in mapM_ (test reducedType) typeMatches

      where
        test reducedType match = case (reducedType,match) of
          -- We expected _some_ error and got _some_.
          (Left _typeErr, TypeError)
            -> pure ()

          (Left typErr, _)
            -> expectationFailure . Text.unpack . render . mconcat $
                 [ text "Could not reduce:"
                 , lineBreak
                 , indent1 $ ppType typ
                 , lineBreak
                 , text "With error:"
                 , lineBreak
                 , ppError ppPattern ppType ppExpr $ typErr
                 ]

          (Right redType, TypeError)
            -> expectationFailure . Text.unpack . render $ mconcat
                 [ text "Type reduced to:"
                 , lineBreak
                 , indent1 $ ppType redType
                 , lineBreak
                 , text "But we expected the reducation to fail."
                 ]

          (Right redType, TypeEquals expectedType)
            -> case typeEq emptyCtx emptyBindings underCtx redType expectedType of
                 Left err
                   -> expectationFailure . Text.unpack . render $ mconcat
                        [ text "The type reduced to:"
                        , lineBreak
                        , indent1 $ ppType redType
                        , lineBreak
                        , text "But failed to compare to the expected type:"
                        , lineBreak
                        , indent1 $ ppType expectedType
                        , lineBreak
                        , text "Due to error:"
                        , lineBreak
                        , indent1 $ ppError ppPattern ppType ppExpr err
                        ]

                 Right False
                   -> expectationFailure . Text.unpack . render . mconcat $
                        [ text "The type reduced to:"
                        , lineBreak
                        , indent1 $ ppType redType
                        , lineBreak
                        , text "But does not equal the expected type:"
                        , lineBreak
                        , indent1 $ ppType expectedType
                        ]

                 Right True
                   -> pure ()

          (Right redType, TypeDoesNotEqual notExpectedType)
            -> case typeEq emptyCtx emptyBindings underCtx redType notExpectedType of
                 Left err
                   -> expectationFailure . Text.unpack . render $ mconcat
                        [ text "The type reduced to:"
                        , lineBreak
                        , indent1 $ ppType redType
                        , lineBreak
                        , text "But failed to compare to the unexpected type:"
                        , lineBreak
                        , indent1 $ ppType notExpectedType
                        , lineBreak
                        , text "Due to error:"
                        , lineBreak
                        , indent1 $ ppError ppPattern ppType ppExpr err
                        ]

                 Right False
                   -> pure ()

                 Right True
                   -> expectationFailure . Text.unpack . render . mconcat $
                        [ text "The type reduced to:"
                        , lineBreak
                        , indent1 $ ppType redType
                        , lineBreak
                        , text "Which equals the unexpected type:"
                        , lineBreak
                        , indent1 $ ppType notExpectedType
                        ]

    apply :: Type -> [Type -> Type] -> Type
    apply t fs = foldl (\t f -> f t) t fs

