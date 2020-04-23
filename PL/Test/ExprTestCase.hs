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
  , typeChecksTo

  , manyAppliedReducesToSpec
  , appliedReducesToSpec
  , reduceToSpec
  , uncurry3

  , parseToSpec
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
import PLParser
import PLParser (runParser,Parser,ParseResult(..),pointTo)
import PLParser.Cursor
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

data ExprTestCase = ExprTestCase
  {_underTypeCtx :: TypeCtx DefaultPhase -- ^ Under this given typing context
  ,_isExpr       :: Expr          -- ^ An Expr
  ,_typed        :: Type    -- ^ Has this type
  ,_parsesFrom   :: Text          -- ^ And also parses from this textual representation
  }

-- | Test whether an expression typechecks.
-- Name an expression, check it fully typechecks AND type checks to the given type
typeChecksTo
  :: TypeCtx DefaultPhase
  -> Text
  -> Expr
  -> Type
  -> (Type -> Doc)
  -> Spec
typeChecksTo typeCtx name expr expectTy ppType = it (Text.unpack name) $ case topExprType typeCtx expr of
  Left err
    -> expectationFailure $ Text.unpack $ render $ ppError ppType err

  Right exprTy
    -> case typeEq emptyCtx emptyBindings typeCtx exprTy expectTy of
         Left err -> expectationFailure $ Text.unpack $ render $ text "A given type name does not exist in the context"
         Right False -> expectationFailure $ Text.unpack $ render $ text "Expected: " <> ppType expectTy <> text " got: " <> ppType exprTy
         Right True  -> return ()

-- | Test whether an expression reduces to another.
--
-- Name an expression, apply it to a list of (argnames,argument,expected result) tuples.
-- Where the expression in turn applied to each list of arguments must reduce to the given expected result
manyAppliedReducesToSpec
  :: String
  -> Expr
  -> [(String,[Expr],Expr)]
  -> (Expr -> Doc)
  -> (Type -> Doc)
  -> Spec
manyAppliedReducesToSpec name expr reductions ppExpr ppType = describe name $ mapM_ (\(appName,appArgs,appResult) -> appliedReducesToSpec expr appName appArgs appResult ppExpr ppType) reductions

-- Name an expression, apply it to a list of expressions. Does it reduce to the given expression?
appliedReducesToSpec
  :: Expr
  -> String
  -> [Expr]
  -> Expr
  -> (Expr -> Doc)
  -> (Type -> Doc)
  -> Spec
appliedReducesToSpec expr name apps = reduceToSpec name (appise (expr:apps))

-- Name an expression. Check it reduces to an expression.
reduceToSpec
  :: String
  -> Expr
  -> Expr
  -> (Expr -> Doc)
  -> (Type -> Doc)
  -> Spec
reduceToSpec name expr eqExpr ppExpr ppType = it name $ case reduce expr of
  Left exprErr
    -> expectationFailure $ Text.unpack $ render $ ppError ppType exprErr

  Right redExpr
    -> if redExpr == eqExpr
         then return ()

         -- Doesnt equal initial expression.
         -- reduce that expression and check once more
         else case reduce eqExpr of
                Left eqExprErr
                  -> expectationFailure $ Text.unpack $ render $ mconcat
                       [text "target expression reduces, doesnt match the expected expression AND the expected expression fails to reduce itself:"
                       ,ppError ppType eqExprErr
                       ]

                Right redEqExpr
                  -> if redExpr == redEqExpr
                       then return ()
                       else expectationFailure "target and expected expression both reduce BUT they are not equal"

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- | Test whether some text parses to some expression
parseToSpec
  :: Parser Expr
  -> Text.Text
  -> Text.Text
  -> Expr
  -> (Expr -> Doc)
  -> Spec
parseToSpec testExprP name txt expectExpr ppExpr = it (Text.unpack name) $ case runParser testExprP txt of
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

  ParseSuccess expr c
    -> if expr == expectExpr
         then return ()
         else expectationFailure . Text.unpack
                                 . render
                                 . mconcat
                                 $ [text "Parses successfully, BUT not as expected. Got:"
                                   ,lineBreak
                                   ,ppExpr expr
                                   ,lineBreak
                                   ,text "expected"
                                   ,lineBreak
                                   ,ppExpr expectExpr
                                   ]

-- TODO: Derive parser and printer from grammar
testPipeline
  :: Grammar Expr
  -> Grammar Type
  -> (forall a. Grammar a -> (Parser a,Printer a))
  -> TypeCtx DefaultPhase
  -> Text.Text
  -> String
testPipeline testExprGrammar testTypeGrammar parseprint typeCtx txt =
  let (testExprParser, testExprPrinter) = parseprint testExprGrammar
      (testTypeParser, testTypePrinter) = parseprint testTypeGrammar

      ppExpr = fromMaybe mempty . pprint testExprPrinter
      ppType = fromMaybe mempty . pprint testTypePrinter

   in case runParser testExprParser txt of
        ParseFailure expected c
          -> unlines [ "Parse failure"
                     , "Parse expected: " ++ show expected
                     , Text.unpack $ pointTo (render . ppPos) c
                     ]

        ParseSuccess expr c
          -> case topExprType typeCtx expr of
                Left err
                  -> Text.unpack . render . mconcat . intersperse lineBreak $
                       [ text "Type check failure: "
                       , text "Parses: "
                       , ppExpr expr
                       , text "Type error: "
                       , ppError ppType err
                       ]

                Right exprTy
                  -> case reduce expr of
                       Left err
                         -> "reduce error"

                       Right redExpr
                         -> Text.unpack . render . mconcat . intersperse lineBreak $
                              [text "Success"
                              ,text "Parses:"
                              ,ppExpr expr
                              ,lineBreak

                              ,text "Type checks:"
                              ,ppType exprTy
                              ,lineBreak

                              ,text "Reduces:"
                              ,ppExpr redExpr
                              ]

