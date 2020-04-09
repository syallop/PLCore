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
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

import PLParser (runParser,Parser,ParseResult(..),pointTo)
import PLGrammar
import PLParser
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

type TestType = Type TyVar
type TestMatchArg = MatchArg Var TyVar

data MatchArgTestCase = MatchArgTestCase
  {_underTypeCtx         :: TypeCtx TyVar                     -- ^ Under this given typing context
  ,_underExprBindCtx     :: ExprBindCtx Var TyVar
  ,_underTypeBindCtx     :: TypeBindCtx TyVar
  ,_underTypeBindings    :: TypeBindings TyVar
  ,_isMatchArg           :: TestMatchArg                      -- ^ A MatchArg
  ,_typed                :: Type TyVar                        -- ^ Has this type
  ,_checkMatchWithResult :: Either (Error TyVar) [Type TyVar] -- ^ Either produces an error or a list of bound types.
  ,_parsesFrom           :: Text                              -- ^ And also parses from this textual representation
  }

hasExpectedResultSpec
  :: TypeCtx TyVar
  -> ExprBindCtx Var TyVar
  -> TypeBindCtx TyVar
  -> TypeBindings TyVar
  -> TestMatchArg
  -> Type TyVar
  -> Either (Error TyVar) [Type TyVar]
  -> (TestType -> Doc)
  -> Spec
hasExpectedResultSpec typeCtx exprBindCtx typeBindCtx typeBindings testMatchArg expectTy expect ppType =
  it "Has expected result" $ isExpected (checkMatchWith testMatchArg expectTy exprBindCtx typeBindCtx typeBindings typeCtx) expect
  where
    fromRight :: b -> Either a b -> b
    fromRight _ (Right b) = b
    fromRight b _         = b

    isExpected :: Either (Error TyVar) [Type TyVar] -> Either (Error TyVar) [Type TyVar] -> Expectation
    isExpected result expected = case (result,expected) of
      (Left resultErr, Left expectedErr)
        | resultErr == expectedErr -> return ()
        | otherwise  -> expectationFailure $ Text.unpack $ render $ mconcat
            [ text "MatchArg expected error:"
            , ppError ppType expectedErr
            , text "but got:"
            , ppError ppType resultErr
            ]

      (Right resultTys, Right expectedTys)
        | length resultTys == length expectedTys
          && all (fromRight False . uncurry (typeEq typeBindCtx typeBindings typeCtx)) (zip resultTys expectedTys)
          -> return ()

        | otherwise
          -> expectationFailure $ Text.unpack $ render $ mconcat
               [ text "MatchArg expected to bind:"
               , foldr ((<>) . ppType) mempty expectedTys
               , text "but bound:"
               , foldr ((<>) . ppType) mempty resultTys
               ]

      (Right resultTys, Left expectedErr)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "MatchArg expected error:"
             , ppError ppType expectedErr
             , text "but got successful result, binding types:"
             , foldr ((<>) . ppType) mempty resultTys
             ]

      (Left resultErr, Right expectedTys)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "MatchArg expected to bind:"
             , foldr ((<>) . ppType) mempty expectedTys
             , text "but got error:"
             , ppError ppType resultErr
             ]

parseToSpec
  :: Parser TestMatchArg
  -> Text.Text
  -> Text.Text
  -> TestMatchArg
  -> (TestMatchArg -> Doc)
  -> Spec
parseToSpec testMatchArgP name txt expectMatchArg ppMatchArg = it (Text.unpack name) $ case runParser testMatchArgP txt of
  (f@(ParseFailure failures cursor))
    -> expectationFailure $ Text.unpack $ render $ mconcat $
         [ text "Parse failure at:"
         , lineBreak

         , indent1 $ ppCursor cursor
         , lineBreak
         ]
         ++
         if null failures
           then mempty
           else [ text "The failures backtracked from were:"
                , lineBreak
                , indent1 . mconcat
                          . map (\(cursor,expected) -> mconcat [ ppCursor cursor
                                                               , ppExpected expected
                                                               , lineBreak
                                                               , lineBreak
                                                               ]
                                )
                          . Map.toList
                          . collectFailures
                          $ failures
                ]


  ParseSuccess matchArg cursor
    -> when (matchArg /= expectMatchArg)
         $ expectationFailure . Text.unpack
                              . render
                              . mconcat
                              $ [ text "Parses successfully, BUT not as expected. Got:"
                                , lineBreak
                                , ppMatchArg matchArg
                                , lineBreak
                                , text "expected"
                                , lineBreak
                                , ppMatchArg expectMatchArg
                                ]

