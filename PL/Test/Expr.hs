{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
{-|
Module      : PL.Test.Expr
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr
-}
module PL.Test.Expr where

-- Abstracts the pattern of testing expressions parsing, reducing and typechecking
import PL.Test.ExprTestCase

-- Some specific ExprSpec tests
import PL.Test.Expr.BigLam
import PL.Test.Expr.Boolean
import PL.Test.Expr.Function
import PL.Test.Expr.Lam
import PL.Test.Expr.List
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Kind
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
import PL.Test.Source

-- type context of bools and nats
typeCtx :: TypeCtx TyVar
typeCtx = foldr (unionTypeCtx . fromJust) emptyTypeCtx
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]

-- A record of the sources required to run all of the TestExpr tests.
data TestExprSources = TestExprSources
  { _lamTestCases      :: TestLamSources
  , _bigLamTestCases   :: TestBigLamSources
  , _booleanTestCases  :: TestBooleanSources
  , _naturalTestCases  :: TestNaturalSources
  , _sumTestCases      :: TestSumSources
  , _productTestCases  :: TestProductSources
  , _unionTestCases    :: TestUnionSources
  , _functionTestCases :: TestFunctionSources
  , _listTestCases     :: TestListSources
  }

-- A List of named TestCase structures for testing expressions depends upon a
-- collection of sources.
testCases
  :: TestExprSources
  -> [(Text.Text, ExprTestCase)]
testCases t = mconcat
  [ lamTestCases      . _lamTestCases      $ t
  , bigLamTestCases   . _bigLamTestCases   $ t
  , booleanTestCases  . _booleanTestCases  $ t
  , naturalTestCases  . _naturalTestCases  $ t
  , sumTestCases      . _sumTestCases      $ t
  , productTestCases  . _productTestCases  $ t
  , unionTestCases    . _unionTestCases    $ t
  , functionTestCases . _functionTestCases $ t
  , listTestCases     . _listTestCases     $ t
  ]

-- Define:
--
-- spec :: Spec
-- spec = parserSpec YOURPARSER
--
-- to define a spec testing your Expr parser.
parserSpec
  :: TestExprSources
  -> Parser TestExpr
  -> (TestExpr -> Doc)
  -> (TestType -> Doc)
  -> Spec
parserSpec testSources testExprP ppExpr ppType
  = describe "Expressions using Var for binding and Type for abstraction" $ sequence_
  [ typeChecksSpec testSources ppType
  , reducesToSpec testSources ppExpr ppType
  , parsesToSpec testSources testExprP ppExpr
  ]

-- Check that expressions type check and type check to a specific type
typeChecksSpec
  :: TestExprSources
  -> (TestType -> Doc)
  -> Spec
typeChecksSpec testSources ppType
  = describe "fully typecheck AND typechecks to the correct type"
  . mapM_ (\(name,testCase) -> typeChecksTo (_underTypeCtx testCase)
                                            name
                                            (_isExpr testCase)
                                            (_typed testCase)
                                            ppType
          )
  $ testCases testSources

-- Test that expressions reduce to an expected expression when applied to lists of arguments
reducesToSpec
  :: TestExprSources
  -> (TestExpr -> Doc)
  -> (TestType -> Doc)
  -> Spec
reducesToSpec testSources ppExpr ppType
  = describe "when applied to a list of arguments must reduce to an expected expression"
  . mapM_ (\(name,testCase,reductionTests) -> manyAppliedReducesToSpec name
                                                                       (_isExpr testCase)
                                                                       reductionTests
                                                                       ppExpr
                                                                       ppType
          )
  $
  [("boolean and"
   ,(andExprTestCase . _andTestCase . _booleanTestCases $ testSources)
   ,[ andOneTrue
    , andFalseTrue
    , andTrueTrue
    ])

  ,("subtract 2"
   , (subTwoExprTestCase . _subTwoTestCase . _naturalTestCases $ testSources)
   ,[ ("3 - 2 ~> 1", [three], one)
    , ("2 - 2 ~> 0", [two]  , zero)
    , ("1 - 2 ~> 0", [one]  , zero)
    ])

  ,("sum expressions"
   , (sumThreeExprTestCase . _sumThreeTestCase . _sumTestCases $ testSources)
   , [ ("+1 False ~> 0", [fixExpr $ Sum falseTerm 1 [natTypeName,boolTypeName,natTypeName]], zero)
     , ("+0 0     ~> 0", [fixExpr $ Sum zero      0 [natTypeName,boolTypeName,natTypeName]], zero)
     , ("+2 0     ~> 1", [fixExpr $ Sum zero      2 [natTypeName,boolTypeName,natTypeName]], one)
     ])

  ,("product expressions"
   , (productThreeExprTestCase . _productThreeTestCase . _productTestCases $ testSources)
   , [ ("* 1 True  0 ~> True" , [fixExpr $ Product [one,trueTerm,zero]] , trueTerm)
     , ("* 1 False 0 ~> False", [fixExpr $ Product [one,falseTerm,zero]], falseTerm)
     , ("* 1 True  1 ~> False", [fixExpr $ Product [one,trueTerm,one]]  , falseTerm)
     , ("* 4 False 1 ~> False", [fixExpr $ Product [four,falseTerm,one]], falseTerm)
     ])

  ,("union expressions"
   ,(unionTwoExprTestCase . _unionTwoTestCase . _unionTestCases $ testSources)
   ,[("∪ 1     Nat   Nat Bool ~> True"                                   , [fixExpr $ Union one       natTypeName  $ Set.fromList [natTypeName,boolTypeName]], trueTerm)
    ,("∪ False Bool  Nat Bool ~> False"                                  , [fixExpr $ Union falseTerm boolTypeName $ Set.fromList [natTypeName,boolTypeName]], falseTerm)
    ,("∪ True  Bool  Bool Nat ~> False -- order of union does not matter", [fixExpr $ Union falseTerm boolTypeName $ Set.fromList [boolTypeName,natTypeName]], falseTerm)
    ])
  ]
  where

    -- name list of args applied to andExpr and the expected result
    andOneTrue, andFalseTrue, andTrueTrue :: (String,[TestExpr],TestExpr)

    andOneTrue   = ("True       ~> Boolean identity function" , [trueTerm] , andOneTrueExpr)
      where andOneTrueExpr = fixExpr $ Lam boolTypeName $ fixExpr $ CaseAnalysis $ Case (fixExpr $ Binding VZ) $ CaseBranches
                               (CaseBranch falsePat falseTerm :| []
                               )
                               (Just trueTerm)

    andFalseTrue = ("False True ~> False", [falseTerm, trueTerm], falseTerm)
    andTrueTrue  = ("True True  ~> True" , [trueTerm, trueTerm] , trueTerm)





-- Test that Text strings parse to an expected expression
parsesToSpec
  :: TestExprSources
  -> Parser TestExpr
  -> (TestExpr -> Doc)
  -> Spec
parsesToSpec sources testExprP ppExpr
  = describe "are parsed by given strings"
  . mapM_ (\(name,testCase) -> parseToSpec testExprP name (_parsesFrom testCase) (_isExpr testCase) ppExpr)
  . testCases
  $ sources

