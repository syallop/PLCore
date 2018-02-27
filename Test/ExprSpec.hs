{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr
-}
module ExprSpec where

-- Abstracts the pattern of testing expressions parsing, reducing and typechecking
import ExprTestCase

-- Some specific ExprSpec tests
import ExprSpec.BigLam
import ExprSpec.Boolean
import ExprSpec.Function
import ExprSpec.Lam
import ExprSpec.List
import ExprSpec.Natural
import ExprSpec.Product
import ExprSpec.Sum
import ExprSpec.Union

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Kind
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser
import PLPrinter

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text

import Test.Hspec

-- type context of bools and nats
typeCtx :: TypeCtx TyVar
typeCtx = foldr (unionTypeCtx . fromJust) emptyTypeCtx
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]
-- A List of named TestCase structures
testCases :: [(Text.Text,ExprTestCase)]
testCases = mconcat
  [lamTestCases
  ,bigLamTestCases
  ,[("booleans"      , andExprTestCase)
   ,("naturals"      , subTwoExprTestCase)
   ,("sum types"     , sumThreeExprTestCase)
   ,("product types" , productThreeExprTestCase)
   ,("union types"   , unionTwoExprTestCase)
   ,("id"            , idExprTestCase)
   ,("const"         , constExprTestCase)
   ,("apply"         , applyExprTestCase)
   {-,("list of nats"  , listNatExprTestCase)-}
   ]
  ]

spec :: Spec
spec = describe "Expressions using Var for binding and Type for abstraction" $ sequence_
  [typeChecksSpec
  ,reducesToSpec
  ,parsesToSpec
  ]

-- Check that expressions type check and type check to a specific type
typeChecksSpec :: Spec
typeChecksSpec = describe "fully typecheck AND typechecks to the correct type"
               . mapM_ (\(name,testCase) -> typeChecksTo (_underTypeCtx testCase) name (_isExpr testCase) (_typed testCase))
               $ testCases

-- Test that expressions reduce to an expected expression when applied to lists of arguments
reducesToSpec :: Spec
reducesToSpec = describe "when applied to a list of arguments must reduce to an expected expression"
              . mapM_ (\(name,testCase,reductionTests) -> manyAppliedReducesToSpec name (_isExpr testCase) reductionTests)
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
   ,[("+1 False ~> 0", [fixExpr $ Sum falseTerm 1 [natTypeName,boolTypeName,natTypeName]], zero)
    ,("+0 0     ~> 0", [fixExpr $ Sum zero      0 [natTypeName,boolTypeName,natTypeName]], zero)
    ,("+2 0     ~> 1", [fixExpr $ Sum zero      2 [natTypeName,boolTypeName,natTypeName]], one)
    ])

  ,("product expressions"
   ,productThreeExprTestCase
   ,[("* 1 True  0 ~> True" , [fixExpr $ Product [one,trueTerm,zero]] , trueTerm)
    ,("* 1 False 0 ~> False", [fixExpr $ Product [one,falseTerm,zero]], falseTerm)
    ,("* 1 True  1 ~> False", [fixExpr $ Product [one,trueTerm,one]]  , falseTerm)
    ,("* 4 False 1 ~> False", [fixExpr $ Product [four,falseTerm,one]], falseTerm)
    ])

  ,("union expressions"
   ,unionTwoExprTestCase
   ,[("∪ 1     Nat   Nat Bool ~> True"                                   , [fixExpr $ Union one       natTypeName  $ Set.fromList [natTypeName,boolTypeName]], trueTerm)
    ,("∪ False Bool  Nat Bool ~> False"                                  , [fixExpr $ Union falseTerm boolTypeName $ Set.fromList [natTypeName,boolTypeName]], falseTerm)
    ,("∪ True  Bool  Bool Nat ~> False -- order of union does not matter", [fixExpr $ Union falseTerm boolTypeName $ Set.fromList [boolTypeName,natTypeName]], falseTerm)
    ])
  ]
  where

    -- name list of args applied to andExpr and the expected result
    andOneTrue,andFalseTrue,andTrueTrue :: (String,[TestExpr],TestExpr)

    andOneTrue   = ("True       ~> Boolean identity function" , [trueTerm] , andOneTrueExpr)
      where andOneTrueExpr = fixExpr $ Lam boolTypeName $ fixExpr $ CaseAnalysis $ Case (fixExpr $ Binding VZ) $ CaseBranches
                               (CaseBranch falsePat falseTerm :| []
                               )
                               (Just trueTerm)

    andFalseTrue = ("False True ~> False", [falseTerm, trueTerm], falseTerm)
    andTrueTrue  = ("True True  ~> True" , [trueTerm, trueTerm] , trueTerm)





-- Test that Text strings parse to an expected expression
parsesToSpec :: Spec
parsesToSpec = describe "are parsed by given strings"
             . mapM_ (\(name,testCase) -> parseToSpec name (_parsesFrom testCase) (_isExpr testCase))
             $ testCases


