{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr
-}
module ExprSpec where

import ExprTestCase
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

-- Check that expressions type check and type check to a specific type
typeChecksSpec :: Spec
typeChecksSpec = describe "An expression fully typechecks AND typechecks to the correct type"
               . sequence_
               . map (\(name,testCase) -> typeChecksTo typeCtx name (_isExpr testCase) (_typed testCase))
               $
  [("booleans"      , andExprTestCase)
  ,("naturals"      , subTwoExprTestCase)
  ,("sum types"     , sumThreeExprTestCase)
  ,("product types" , productThreeExprTestCase)
  ,("union types"   , unionTwoExprTestCase)
  ,("id"            , idExprTestCase)
  ,("const"         , constExprTestCase)
  ,("list of nats"  , listNatExprTestCase)
  ]
  where

-- Test that expressions reduce to an expected expression when applied to lists of arguments
reducesToSpec :: Spec
reducesToSpec = describe "An expression when applied to a list of arguments must reduce to an expected expression"
              . sequence_
              . map (\(name,testCase,reductionTests) -> manyAppliedReducesToSpec name (_isExpr testCase) reductionTests)
              $
  [("boolean and"
   ,andExprTestCase
   ,[andOneTrue
    ,andFalseTrue
    ,andTrueTrue
    ])

  ,("subtract 2"
   ,subTwoExprTestCase
   ,[("3 - 2 ~> 1", [three], one)
    ,("2 - 2 ~> 0", [two]  , zero)
    ,("1 - 2 ~> 0", [one]  , zero)
    ])

  ,("sum expressions"
   ,sumThreeExprTestCase
   ,[("+1 False ~> 0", [Sum falseTerm 1 [natTypeName,boolTypeName,natTypeName]], zero)
    ,("+0 0     ~> 0", [Sum zero      0 [natTypeName,boolTypeName,natTypeName]], zero)
    ,("+2 0     ~> 1", [Sum zero      2 [natTypeName,boolTypeName,natTypeName]], one)
    ])

  ,("product expressions"
   ,productThreeExprTestCase
   ,[("* 1 True  0 ~> True" , [Product [one,trueTerm,zero]] , trueTerm)
    ,("* 1 False 0 ~> False", [Product [one,falseTerm,zero]], falseTerm)
    ,("* 1 True  1 ~> False", [Product [one,trueTerm,one]]  , falseTerm)
    ,("* 4 False 1 ~> False", [Product [four,falseTerm,one]], falseTerm)
    ])

  ,("union expressions"
   ,unionTwoExprTestCase
   ,[("∪ 1     Nat   Nat Bool ~> True"                                   , [Union one       natTypeName  $ Set.fromList [natTypeName,boolTypeName]], trueTerm)
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





-- Test that Text strings parse to an expected expression
parsesToSpec :: Spec
parsesToSpec = describe "Strings should parse to expected expressions"
             . sequence_
             . map (\(name,testCase) -> parseToSpec name (_parsesFrom testCase) (_isExpr testCase))
             $
  [("boolean and"       , andExprTestCase)
  ,("sutract two"       , subTwoExprTestCase)
  ,("sum expression"    , sumThreeExprTestCase)
  ,("product expression", productThreeExprTestCase)
  ,("union expression"  , unionTwoExprTestCase)
  ]
  where

-- type context of bools and nats
typeCtx :: TypeCtx TyVar
typeCtx = foldr unionTypeCtx emptyTypeCtx . map fromJust $
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]

