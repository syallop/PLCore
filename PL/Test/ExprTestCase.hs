{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
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
  ( TestExpr
  , TestType
  , ExprTestCase(..)
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
import PL.FixExpr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.FixType
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

import PLGrammar
import PLParser
import PLParser (runParser,Parser,ParseResult(..),pointTo)
import PLParser.Cursor
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
type TestExpr = Expr Var TestType TyVar

data ExprTestCase = ExprTestCase
  {_underTypeCtx :: TypeCtx TyVar -- ^ Under this given typing context
  ,_isExpr       :: TestExpr      -- ^ An Expr
  ,_typed        :: Type TyVar    -- ^ Has this type
  ,_parsesFrom   :: Text          -- ^ And also parses from this textual representation
  }

-- | Test whether an expression typechecks.
-- Name an expression, check it fully typechecks AND type checks to the given type
typeChecksTo
  :: ( Document TestExpr
     , Document TestType
     )
  => TypeCtx TyVar
  -> Text
  -> TestExpr
  -> TestType
  -> Spec
typeChecksTo typeCtx name expr expectTy = it (Text.unpack name) $ case topExprType typeCtx expr of
  Left err
    -> expectationFailure $ Text.unpack $ renderDocument err

  Right exprTy
    -> case typeEq emptyCtx emptyBindings typeCtx exprTy expectTy of
         Left err -> expectationFailure $ Text.unpack $ render $ text "A given type name does not exist in the context"
         Right False -> expectationFailure $ Text.unpack $ render $ text "Expected: " <> document expectTy <> text " got: " <> document exprTy
         Right True  -> return ()

-- | Test whether an expression reduces to another.
--
-- Name an expression, apply it to a list of (argnames,argument,expected result) tuples.
-- Where the expression in turn applied to each list of arguments must reduce to the given expected result
manyAppliedReducesToSpec
  :: ( Document TestExpr
     , Document TestType
     , Document (ExprF Var TestType TyVar (FixExpr Var TestType TyVar ExprF))
     )
  => String
  -> TestExpr
  -> [(String,[TestExpr],TestExpr)]
  -> Spec
manyAppliedReducesToSpec name expr reductions = describe name $ mapM_ (\(appName,appArgs,appResult) -> appliedReducesToSpec expr appName appArgs appResult) reductions

-- Name an expression, apply it to a list of expressions. Does it reduce to the given expression?
appliedReducesToSpec
  :: ( Document TestExpr
     , Document TestType
     , Document (ExprF Var TestType TyVar (FixExpr Var TestType TyVar ExprF))
     )
  => TestExpr
  -> String
  -> [TestExpr]
  -> TestExpr
  -> Spec
appliedReducesToSpec expr name apps = reduceToSpec name (appise (expr:apps))

-- Name an expression. Check it reduces to an expression.
reduceToSpec
  :: ( Document TestExpr
     , Document TestType
     , Document (Type TyVar)
     , Document (ExprF Var TestType TyVar (FixExpr Var TestType TyVar ExprF))
     )
  => String
  -> TestExpr
  -> TestExpr
  -> Spec
reduceToSpec name expr eqExpr = it name $ case reduce expr of
  Left exprErr
    -> expectationFailure $ Text.unpack $ render $ document $ exprErr

  Right redExpr
    -> if redExpr == eqExpr
         then return ()

         -- Doesnt equal initial expression.
         -- reduce that expression and check once more
         else case reduce eqExpr of
                Left eqExprErr
                  -> expectationFailure $ Text.unpack $ render $ mconcat
                       [text "target expression reduces, doesnt match the expected expression AND the expected expression fails to reduce itself:"
                       ,document eqExprErr
                       ]

                Right redEqExpr
                  -> if redExpr == redEqExpr
                       then return ()
                       else expectationFailure "target and expected expression both reduce BUT they are not equal"

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

{-testExprP :: Parser TestExpr-}
{-testExprP = toParser $ expr var (typ tyVar) tyVar-}

-- | Test whether some text parses to some expression
parseToSpec
  :: ( Document (ParseResult TestExpr)
     , Document TestExpr
     )
  => Parser TestExpr
  -> Text.Text
  -> Text.Text
  -> TestExpr
  -> Spec
parseToSpec testExprP name txt expectExpr = it (Text.unpack name) $ case runParser testExprP txt of
  (f@(ParseFailure e c))
    -> expectationFailure $ Text.unpack $ render $ document f

  ParseSuccess expr c
    -> if expr == expectExpr
         then return ()
         else expectationFailure . Text.unpack
                                 . render
                                 . mconcat
                                 $ [text "Parses successfully, BUT not as expected. Got:"
                                   ,lineBreak
                                   ,document expr
                                   ,lineBreak
                                   ,text "expected"
                                   ,lineBreak
                                   ,document expectExpr
                                   ]

testPipeline
  :: ( Document (ParseResult TestExpr)
     , Document Pos
     , Document TestExpr
     , Document TestType
     )
  => Parser TestExpr
  -> TypeCtx TyVar
  -> Text.Text
  -> String
testPipeline testExprP typeCtx txt = case runParser testExprP txt of
  ParseFailure expected c
    -> unlines [ "Parse failure"
               , "Parse expected: " ++ show expected
               , Text.unpack $ pointTo renderDocument c
               ]

  ParseSuccess expr c
    -> case topExprType typeCtx expr of
          Left err
            -> Text.unpack . render . mconcat . intersperse lineBreak $
                 [ text "Type check failure: "
                 , text "Parses: "
                 , document expr
                 , text "Type error: "
                 , document err
                 ]

          Right exprTy
            -> case reduce expr of
                 Left err
                   -> "reduce error"

                 Right redExpr
                   -> Text.unpack . render . mconcat . intersperse lineBreak $
                        [text "Success"
                        ,text "Parses:"
                        ,document expr
                        ,lineBreak

                        ,text "Type checks:"
                        ,document exprTy
                        ,lineBreak

                        ,text "Reduces:"
                        ,document redExpr
                        ]

