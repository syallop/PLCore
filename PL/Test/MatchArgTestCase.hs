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

import Test.Hspec
import PL.Test.Source

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
  :: Document (ParseResult TestMatchArg)
  => TypeCtx TyVar
  -> ExprBindCtx Var TyVar
  -> TypeBindCtx TyVar
  -> TypeBindings TyVar
  -> TestMatchArg
  -> Type TyVar
  -> Either (Error TyVar) [Type TyVar]
  -> Spec
hasExpectedResultSpec typeCtx exprBindCtx typeBindCtx typeBindings testMatchArg expectTy expect =
  it "Has expected result" $ isExpected (checkMatchWith testMatchArg expectTy exprBindCtx typeBindCtx typeBindings typeCtx) expect
  where
    isExpected :: Either (Error TyVar) [Type TyVar] -> Either (Error TyVar) [Type TyVar] -> Expectation
    isExpected result expected = case (result,expected) of
      (Left resultErr, Left expectedErr)
        | resultErr == expectedErr -> return ()
        | otherwise  -> expectationFailure $ Text.unpack $ render $ mconcat
            [ text "MatchArg expected error:"
            , document expectedErr
            , text "but got:"
            , document resultErr
            ]

      (Right resultTys, Right expectedTys)
        | length resultTys == length expectedTys
          && all (fromMaybe False . uncurry (typeEq typeBindCtx typeBindings typeCtx)) (zip resultTys expectedTys)
          -> return ()

        | otherwise
          -> expectationFailure $ Text.unpack $ render $ mconcat
               [ text "MatchArg expected to bind:"
               , foldr ((<>) . document) mempty expectedTys
               , text "but bound:"
               , foldr ((<>) . document) mempty resultTys
               ]

      (Right resultTys, Left expectedErr)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "MatchArg expected error:"
             , document expectedErr
             , text "but got successful result, binding types:"
             , foldr ((<>) . document) mempty resultTys
             ]

      (Left resultErr, Right expectedTys)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "MatchArg expected to bind:"
             , foldr ((<>) . document) mempty expectedTys
             , text "but got error:"
             , document resultErr
             ]

parseToSpec
  :: Document (ParseResult TestMatchArg)
  => Parser TestMatchArg
  -> Text.Text
  -> Text.Text
  -> TestMatchArg
  -> Spec
parseToSpec testMatchArgP name txt expectMatchArg =
  it (Text.unpack name) $ case runParser testMatchArgP txt of
    (f@(ParseFailure e c))
      -> expectationFailure $ Text.unpack $ render $ document f

    ParseSuccess matchArg c
      -> when (matchArg /= expectMatchArg)
           $ expectationFailure . Text.unpack
                                . render
                                . mconcat
                                $ [text "Parses successfully, BUT not as expected. Got:"
                                  ,lineBreak
                                  ,document matchArg
                                  ,lineBreak
                                  ,text "expected"
                                  ,lineBreak
                                  ,document expectMatchArg
                                  ]

