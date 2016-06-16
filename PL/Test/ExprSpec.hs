{-# LANGUAGE OverloadedStrings #-}
module ExprSpec where

import PL.Error
import PL.Expr
import PL.Reduce
import PL.Type
import PL.TypeCtx
import PL.Parser
import PL.Var
import PL.TyVar
import PL.Parser.Lispy hiding (appise,lamise)

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text

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
  [("booleans"     , andExpr         ,andExprType)
  ,("naturals"     , subTwoExpr      ,subTwoExprType)
  ,("sum types"    , sumThreeExpr    ,sumThreeExprType)
  ,("product types", productThreeExpr,productThreeExprType)
  ,("union types"  , unionTwoExpr    ,unionTwoExprType)
  ]
  where
    -- Name an expression, check it fully typechecks AND type checks to the given type
    typeChecksTo :: String -> TestExpr -> TestType -> Spec
    typeChecksTo name expr ty = it name $ topExprType typeCtx expr `shouldSatisfy` (either (const False) (\exprTy -> case typeEq ty exprTy typeCtx of
                                                                                                            Nothing -> False -- A Name doesnt exit
                                                                                                            Just t  -> t
                                                                                                         )
                                                                                   )

-- Test that expressions reduce to an expected expression when applied to lists of arguments
reducesToSpec :: Spec
reducesToSpec = describe "An expression when applied to a list of arguments must reduce to an expected expression" $ sequence_ . map (uncurry3 manyAppliedReducesToSpec) $
  [("boolean and" ,andExpr                ,[andOneTrue
                                           ,andFalseTrue
                                           ,andTrueTrue
                                           ])

  ,("subtract 2"  ,subTwoExpr             ,[("3 - 2 ~> 1", [three], one)
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
      where andOneTrueExpr = Lam boolTypeName $ Case (Binding VZ) $ CaseBranches
                               (SomeCaseBranches (CaseBranch falsePat falseTerm)
                               []
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
        -> expectationFailure ("target expression does not reduce: " ++ show exprErr)

      Right redExpr
        -> if redExpr == eqExpr
             then return ()

             -- Doesnt equal initial expression.
             -- reduce that expression and check once more
             else case reduce eqExpr of
                    Left eqExprErr
                      -> expectationFailure ("target expression reduces, doesnt match the expected expression AND the expected expression fails to reduce itself:" ++ show eqExprErr)

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
        -> expectationFailure ("Unexpected parse failure: " ++ show e ++ "\n" ++ (Text.unpack $ pointTo c))

      ParseSuccess expr c
        -> if expr == expectExpr
             then return ()
             else expectationFailure ("Parses successfully, BUT not as expected. Got:\n" ++ show expr ++ "\n expected:\n" ++ show expectExpr)



{- Booleans -}
boolTypeCtx = insertType "Bool" boolType emptyTypeCtx
boolTypeName = Named "Bool"
boolType    = SumT boolSumType
boolSumType = [ProductT []
              ,ProductT []
              ]
falseTerm     = Sum (Product []) 0 boolSumType
trueTerm      = Sum (Product []) 1 boolSumType
falsePat      = MatchSum 0 (MatchProduct [])
truePat       = MatchSum 1 (MatchProduct [])
falseTermText = "+0(*) (*) (*)"
trueTermText  = "+1(*) (*) (*)"
falsePatText  = "+0(*)"
truePatText   = "+1(*)"

{- Natural numbers -}
natTypeCtx = insertRecType "Nat" natType emptyTypeCtx
natTypeName = Named "Nat"
natType    = SumT natSumType
natSumType = [ProductT []
             ,Named "Nat"
             ]
zTerm      =                     Sum (Product [])        0 natSumType
sTerm      = Lam (Named "Nat") $ Sum (Binding (mkVar 0)) 1 natSumType
zPat       = MatchSum 0 (MatchProduct [])
sPat p     = MatchSum 1 p
zTermText  = "+0(*) (*) Nat"
sTermText  = "\\Nat (+1 0 (*) Nat)"
zPatText   = "+0(*)"
sPatText p = "+1"<>p

zero  = zTerm
suc n = sTerm `App` n
one   = suc zero
two   = suc one
three = suc two
four  = suc three


-- type context of bools and nats
typeCtx :: TypeCtx TyVar
typeCtx = fromJust $ liftA2 unionTypeCtx boolTypeCtx natTypeCtx


-- Boolean and
andExpr :: TestExpr
andExpr = Lam boolTypeName $ Lam boolTypeName $     -- \x:Bool y:Bool ->
    Case (Binding VZ)                               -- case y of
      $ CaseBranches                                --
        (SomeCaseBranches                           --
            (CaseBranch falsePat falseTerm)         --    False -> False
            []                                      --
        )                                           --
        (Just                                       --     _      ->
            (Case (Binding $ VS VZ)                 --               case x of
              $ CaseBranches                        --
                (SomeCaseBranches                   --
                    (CaseBranch falsePat falseTerm) --                 False -> False
                    []                              --
                )                                   --
                (Just                               --                        _     ->
                    trueTerm                        --                                 True
                )
            )
        )
andExprType :: TestType
andExprType = Arrow boolType (Arrow boolType boolType)
andText :: Text.Text
andText = Text.unlines
  ["\\Bool Bool (CASE 0"
  ,"               (| "<>falsePatText<>" "<>falseTermText<>")"
  ,""
  ,"               (CASE 1"
  ,"                   (| "<>falsePatText<>" "<>falseTermText<>")"
  ,""
  ,"                   "<>trueTermText<>""
  ,""
  ,"               )"
  ,"            )"
  ]

-- Test nested pattern matching
-- n > 2     ~> n-2
-- otherwise ~> 0
subTwoExpr :: TestExpr
subTwoExpr = Lam natTypeName $                           -- \n : Nat ->
    Case (Binding VZ)                                    -- case n of
      $ CaseBranches                                     --
        (SomeCaseBranches                                --
            (CaseBranch (sPat $ sPat Bind) (Binding VZ)) --   S S n -> n
            []                                           --
        )                                                --
        (Just                                            --
            zTerm                                        --   _     -> Z
        )
subTwoExprType :: TestType
subTwoExprType = Arrow natType natType
subTwoText :: Text.Text
subTwoText = Text.unlines
  ["\\Nat (CASE 0"
  ,"         (|"<>(sPatText $ sPatText "?")<>" 0)"
  ,"         "<>zTermText
  ,"     )"
  ]

-- Test case analysis on a sum type with overlapping members
sumThreeExpr :: TestExpr
sumThreeExpr = Lam (SumT [natTypeName,boolTypeName,natTypeName]) $   -- \x : Nat|Bool|Nat ->
    Case (Binding VZ)                                                -- case x of
      $ CaseBranches                                                 --
        (SomeCaseBranches                                            --
            (CaseBranch (MatchSum 0 $ sPat Bind) (Binding VZ))       --  0| S n   -> n
            [CaseBranch (MatchSum 0   zPat)      zTerm               --  0| Z     -> Z
            ,CaseBranch (MatchSum 1   falsePat)  zTerm               --  1| False -> Z
            ,CaseBranch (MatchSum 1   truePat)   (sTerm `App` zTerm) --  1| True  -> S Z
            ,CaseBranch (MatchSum 2 $ sPat Bind) zTerm               --  2| S n   -> Z
            ,CaseBranch (MatchSum 2   zPat)      (sTerm `App` zTerm) --  2| Z     -> S Z
            ]
        )
        Nothing
sumThreeExprType :: TestType
sumThreeExprType = Arrow (SumT [natTypeName,boolTypeName,natTypeName]) natTypeName
sumThreeText :: Text.Text
sumThreeText = Text.unlines
  ["\\(+Nat Bool Nat) (CASE 0"
  ,"                   (| (+0 +1 ?)    (0))"
  ,"                   (| (+0 +0 (*))  (+0 (*) (*) Nat))"
  ,"                   (| (+1 +0 (*))  (+0 (*) (*) Nat))"
  ,"                   (| (+1 +1 (*))  (@ (\\Nat (+1 0 (*) Nat))  (+0 (*) (*) Nat) ))"
  ,"                   (| (+2 +1 ?)    (+0 (*) (*) Nat))"
  ,"                   (| (+2 +0 (*))  (@ (\\Nat (+1 0 (*) Nat))  (+0 (*) (*) Nat) ))"
  ,"                 )"
  ]

-- Test product expressions
productThreeExpr :: TestExpr
productThreeExpr = Lam (ProductT [natTypeName,boolTypeName,natTypeName]) $ -- \x : Nat*Bool*Nat ->
    Case (Binding VZ)                                                      -- case x of
      $ CaseBranches                                                       --
        (SomeCaseBranches                                                  --
            (CaseBranch (MatchProduct [zPat,Bind,zPat]) (Binding VZ))      -- Z,y,Z -> y
            [CaseBranch (MatchProduct [Bind,Bind,zPat]) (Binding VZ)]      -- x,y,Z -> y
        )                                                                  --
        (Just                                                              --
            falseTerm                                                      -- _ -> False
        )
productThreeExprType :: TestType
productThreeExprType = Arrow (ProductT [natTypeName,boolTypeName,natTypeName]) boolTypeName
productThreeText :: Text.Text
productThreeText = Text.unlines
  ["\\(* Nat Bool Nat) (CASE 0"
  ,"                    (| (* (+0 (*)) (?) (+0 (*))) (0))"
  ,"                    (| (* (?)      (?) (+0 (*))) (0))"
  ,""
  ,"                    (+0 (*) (*) (*))"
  ,"                  )"
  ]

-- : <Nat|Bool> -> Bool
unionTwoExpr :: TestExpr
unionTwoExpr = Lam (UnionT $ Set.fromList [natTypeName,boolTypeName]) $ -- \x : <Nat|Bool>
    Case (Binding VZ)                                                   -- case x of
      $ CaseBranches                                                    --
        (SomeCaseBranches                                               --
            (CaseBranch (MatchUnion natTypeName   zPat)      falseTerm) -- Nat | Z    -> False
            [CaseBranch (MatchUnion natTypeName $ sPat Bind) trueTerm   -- Nat | S n  -> True
            ,CaseBranch (MatchUnion boolTypeName  truePat)   trueTerm   -- Bool| True -> True
            ]                                                           --
        )                                                               --
        (Just                                                           --
            falseTerm                                                   -- _          -> False
        )
unionTwoExprType :: TestType
unionTwoExprType = Arrow (UnionT $ Set.fromList [natTypeName,boolTypeName]) boolTypeName
unionTwoText :: Text.Text
unionTwoText = Text.unlines
  ["\\(∪ Bool Nat) (CASE 0"
  ,"                (| (∪ Nat  (+0 (*))) (+0 (*) (*) (*)))"
  ,"                (| (∪ Nat  (+1 ?))   (+1 (*) (*) (*)))"
  ,"                (| (∪ Bool (+1 (*))) (+1 (*) (*) (*)))"
  ,""
  ,"                (+0 (*) (*) (*))"
  ,"              )"
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
            -> unlines ["Type check failure"
                       ,"Parses:"
                       ,show expr
                       ,""

                       ,"Type error:"
                       ,show err
                       ]

          Right exprTy
            -> case reduce expr of
                 Left err
                   -> "reduce error"

                 Right redExpr
                   -> unlines ["Success"

                              ,"Parses:"
                              ,show expr
                              ,""

                              ,"Type checks:"
                              ,show exprTy
                              ,""

                              ,"Reduces:"
                              ,show redExpr
                              ]


