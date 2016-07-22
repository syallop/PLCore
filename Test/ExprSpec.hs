{-# LANGUAGE OverloadedStrings #-}
module ExprSpec where

import ExprSpec.Boolean
import ExprSpec.Natural
import ExprSpec.List
import ExprSpec.Sum
import ExprSpec.Product
import ExprSpec.Union
import ExprSpec.Function

import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Parser
import PL.Parser.Lispy hiding (appise,lamise)
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

import PL.Printer

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))
import Data.List

import Test.Hspec

spec :: Spec
spec = describe "Expr Var Type" $ sequence_ [typeChecksSpec,reducesToSpec,parsesToSpec]

type TestType = Type TyVar
type TestExpr = Expr Var TestType TyVar

testExprP :: Parser TestExpr
testExprP = expr var (typ tyVar) tyVar

-- Check that expressions type check and type check to a specific type
typeChecksSpec :: Spec
typeChecksSpec = describe "An expression fully typechecks AND typechecks to the correct type" $ sequence_ . map (uncurry3 typeChecksTo) $
  [("booleans"      , andExpr         ,andExprType)
  ,("naturals"      , subTwoExpr      ,subTwoExprType)
  ,("sum types"     , sumThreeExpr    ,sumThreeExprType)
  ,("product types" , productThreeExpr,productThreeExprType)
  ,("union types"   , unionTwoExpr    ,unionTwoExprType)
  ,("id"            , idExpr          ,idExprType)
  ,("const"         , constExpr       ,constExprType)
  ,("list of nats"  , listNatExpr     ,listNatExprType)
  ]
  where
    -- Name an expression, check it fully typechecks AND type checks to the given type
    typeChecksTo :: String -> TestExpr -> TestType -> Spec
    typeChecksTo name expr expectTy = it name $ case topExprType typeCtx expr of
        Left err
          -> expectationFailure $ Text.unpack $ renderDocument err

        Right exprTy
          -> case typeEq emptyCtx emptyBindings typeCtx exprTy expectTy of
               Nothing    -> expectationFailure $ Text.unpack $ render $ "A given type name does not exist in the context"
               Just False -> expectationFailure $ Text.unpack $ render $ "Expected: " <> document expectTy <> " got: " <> document exprTy
               Just True  -> return ()

-- Test that expressions reduce to an expected expression when applied to lists of arguments
reducesToSpec :: Spec
reducesToSpec = describe "An expression when applied to a list of arguments must reduce to an expected expression" $ sequence_ . map (uncurry3 manyAppliedReducesToSpec) $
  [("boolean and"        ,andExpr         ,[andOneTrue
                                           ,andFalseTrue
                                           ,andTrueTrue
                                           ])

  ,("subtract 2"         ,subTwoExpr      ,[("3 - 2 ~> 1", [three], one)
                                           ,("2 - 2 ~> 0", [two]  , zero)
                                           ,("1 - 2 ~> 0", [one]  , zero)
                                           ])

  ,("sum expressions"    ,sumThreeExpr    ,[("+1 False ~> 0", [Sum falseTerm 1 [natTypeName,boolTypeName,natTypeName]], zero)
                                           ,("+0 0     ~> 0", [Sum zero      0 [natTypeName,boolTypeName,natTypeName]], zero)
                                           ,("+2 0     ~> 1", [Sum zero      2 [natTypeName,boolTypeName,natTypeName]], one)
                                           ])

  ,("product expressions",productThreeExpr,[("* 1 True  0 ~> True" , [Product [one,trueTerm,zero]] , trueTerm)
                                           ,("* 1 False 0 ~> False", [Product [one,falseTerm,zero]], falseTerm)
                                           ,("* 1 True  1 ~> False", [Product [one,trueTerm,one]]  , falseTerm)
                                           ,("* 4 False 1 ~> False", [Product [four,falseTerm,one]], falseTerm)
                                           ])

  ,("union expressions"  ,unionTwoExpr    ,[("∪ 1     Nat   Nat Bool ~> True"                                   , [Union one       natTypeName  $ Set.fromList [natTypeName,boolTypeName]], trueTerm)
                                           ,("∪ False Bool  Nat Bool ~> False"                                  , [Union falseTerm boolTypeName $ Set.fromList [natTypeName,boolTypeName]], falseTerm)
                                           ,("∪ True  Bool  Bool Nat ~> False -- order of union does not matter", [Union falseTerm boolTypeName $ Set.fromList [boolTypeName,natTypeName]], falseTerm)
                                           ])
  ]
  where

    -- name list of args applied to andExpr and the expected result
    andOneTrue,andFalseTrue,andTrueTrue :: (String,[TestExpr],TestExpr)

    andOneTrue   = ("True       ~> Boolean identity function" , [trueTerm] , andOneTrueExpr)
      where andOneTrueExpr = Lam boolTypeName $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
                               (CaseBranch falsePat falseTerm :| []
                               )
                               (Just trueTerm)

    andFalseTrue = ("False True ~> False", [falseTerm, trueTerm], falseTerm)
    andTrueTrue  = ("True True  ~> True" , [trueTerm, trueTerm] , trueTerm)



    -- Name an expression, apply it to a list of (argnames,argument,expected result) tuples.
    -- Where the expression in turn applied to each list of arguments must reduce to the given expected result
    manyAppliedReducesToSpec :: String -> TestExpr -> [(String,[TestExpr],TestExpr)] -> Spec
    manyAppliedReducesToSpec name expr reductions = describe name $ mapM_ (\(appName,appArgs,appResult) -> appliedReducesToSpec expr appName appArgs appResult) $ reductions

    -- Name an expression, apply it to a list of expressions. Does it reduce to the given expression?
    appliedReducesToSpec :: TestExpr -> String -> [TestExpr] -> TestExpr -> Spec
    appliedReducesToSpec expr name apps eqExpr = reduceToSpec name (appise (expr:apps)) eqExpr

    -- Name an expression. Check it reduces to an expression.
    reduceToSpec :: String -> TestExpr -> TestExpr -> Spec
    reduceToSpec name expr eqExpr = it name $ case reduce expr of
      Left exprErr
        -> expectationFailure $ Text.unpack $ render $ "target expression does not reduce: " <> document exprErr

      Right redExpr
        -> if redExpr == eqExpr
             then return ()

             -- Doesnt equal initial expression.
             -- reduce that expression and check once more
             else case reduce eqExpr of
                    Left eqExprErr
                      -> expectationFailure $ Text.unpack $ render $ mconcat ["target expression reduces, doesnt match the expected expression AND the expected expression fails to reduce itself:"
                                                                             ,document eqExprErr
                                                                             ]

                    Right redEqExpr
                      -> if redExpr == redEqExpr
                           then return ()
                           else expectationFailure "target and expected expression both reduce BUT they are not equal"

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(a,b,c) -> f a b c

-- Test that Text strings parse to an expected expression
parsesToSpec :: Spec
parsesToSpec = describe "Strings should parse to expected expressions" $ sequence_ . map (uncurry3 parseToSpec) $
  [("boolean and"       , andText         , andExpr)
  ,("sutract two"       , subTwoText      , subTwoExpr)
  ,("sum expression"    , sumThreeText    , sumThreeExpr)
  ,("product expression", productThreeText, productThreeExpr)
  ,("union expression"  , unionTwoText    , unionTwoExpr)
  ]
  where

    parseToSpec :: String -> Text.Text -> TestExpr -> Spec
    parseToSpec name txt expectExpr = it name $ case runParser testExprP txt of
      ParseFailure e c
        -> expectationFailure $ Text.unpack $ render $ mconcat ["Unexpected parse failure: "
                                                               ,document e
                                                               ,lineBreak
                                                               ,text $ pointTo c
                                                               ]

      ParseSuccess expr c
        -> if expr == expectExpr
             then return ()
             else expectationFailure $ Text.unpack $ render $ mconcat ["Parses successfully, BUT not as expected. Got:"
                                                                      ,lineBreak
                                                                      ,document expr
                                                                      ,lineBreak
                                                                      ,"expected"
                                                                      ,lineBreak
                                                                      ,document expectExpr
                                                                      ]
-- type context of bools and nats
typeCtx :: TypeCtx TyVar
typeCtx = foldr unionTypeCtx emptyTypeCtx . map fromJust $
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]

testPipeline :: Text.Text -> String
testPipeline txt = case runParser testExprP txt of
  ParseFailure expected c
    -> unlines ["Parse failure"
               ,"Parse expected: " ++ show expected
               ,Text.unpack $ pointTo c
               ]

  ParseSuccess expr c
    -> case topExprType typeCtx expr of
          Left err
            -> Text.unpack . render . mconcat . intersperse lineBreak $
                 [ "Type check failure: "
                 , "Parses: "
                 , document expr
                 , "Type error: "
                 , document err
                 ]

          Right exprTy
            -> case reduce expr of
                 Left err
                   -> "reduce error"

                 Right redExpr
                   -> Text.unpack . render . mconcat . intersperse lineBreak $
                        ["Success"
                        ,"Parses:"
                        ,document expr
                        ,""

                        ,"Type checks:"
                        ,document exprTy
                        ,""

                        ,"Reduces:"
                        ,document redExpr
                        ]

