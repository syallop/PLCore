{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , FlexibleContexts
  #-}
{-|
Module      : MatchArgTestCase
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module MatchArgTestCase where

import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.PLParser.Parser (runParser,Parser,ParseResult(..),pointTo)
import PL.PLGrammar.Grammar
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

import PL.PLParser.Parser
import PL.PLPrinter.Printer

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
  -> Spec
hasExpectedResultSpec typeCtx exprBindCtx typeBindCtx typeBindings testMatchArg expectTy expect =
  it "Has expected result" $ isExpected (checkMatchWith testMatchArg expectTy exprBindCtx typeBindCtx typeBindings typeCtx) expect
  where
    isExpected :: Either (Error TyVar) [Type TyVar] -> Either (Error TyVar) [Type TyVar] -> Expectation
    isExpected result expected = case (result,expected) of
      (Left resultErr, Left expectedErr)
        | resultErr == expectedErr -> return ()
        | otherwise  -> expectationFailure $ Text.unpack $ render $ mconcat
            [ DocText "MatchArg expected error:"
            , document expectedErr
            , DocText "but got:"
            , document resultErr
            ]

      (Right resultTys, Right expectedTys)
        | length resultTys == length expectedTys
          && all (fromMaybe False . uncurry (typeEq typeBindCtx typeBindings typeCtx)) (zip resultTys expectedTys)
          -> return ()

        | otherwise
          -> expectationFailure $ Text.unpack $ render $ mconcat
               [ DocText "MatchArg expected to bind:"
               , foldr (DocAppend . document) DocEmpty expectedTys
               , DocText "but bound:"
               , foldr (DocAppend . document) DocEmpty resultTys
               ]

      (Right resultTys, Left expectedErr)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ DocText "MatchArg expected error:"
             , document expectedErr
             , DocText "but got successful result, binding types:"
             , foldr (DocAppend . document) DocEmpty resultTys
             ]

      (Left resultErr, Right expectedTys)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ DocText "MatchArg expected to bind:"
             , foldr (DocAppend . document) DocEmpty expectedTys
             , DocText "but got error:"
             , document resultErr
             ]

parseToSpec
  :: Text.Text
  -> Text.Text
  -> TestMatchArg
  -> Spec
parseToSpec name txt expectMatchArg =
  it (Text.unpack name) $ case runParser testMatchArgP txt of
    (f@(ParseFailure e c))
      -> expectationFailure $ Text.unpack $ render $ document f

    ParseSuccess matchArg c
      -> when (matchArg /= expectMatchArg)
           $ expectationFailure . Text.unpack
                                . render
                                . mconcat
                                $ [DocText "Parses successfully, BUT not as expected. Got:"
                                  ,lineBreak
                                  ,document matchArg
                                  ,lineBreak
                                  ,DocText "expected"
                                  ,lineBreak
                                  ,document expectMatchArg
                                  ]

testMatchArgP :: Parser TestMatchArg
testMatchArgP = toParser $ using var (typ tyVar) tyVar matchArg

