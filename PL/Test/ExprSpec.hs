{-# LANGUAGE OverloadedStrings #-}
module ExprSpec where

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

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))

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
  {-[("list of nats", listNatExpr,listNatExprType)]-}
  where
    -- Name an expression, check it fully typechecks AND type checks to the given type
    typeChecksTo :: String -> TestExpr -> TestType -> Spec
    typeChecksTo name expr expectTy = it name $ case topExprType typeCtx expr of
        Left err
          -> expectationFailure $ show err

        Right exprTy
          -> case typeEq emptyCtx emptyBindings typeCtx exprTy expectTy of
               Nothing    -> expectationFailure "A given type name does not exist in the context"
               Just False -> expectationFailure $ "Expected: " ++ show expectTy ++ " got: " ++ show exprTy
               Just True  -> return ()

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

identityTypeName = Named "Identity"
identityType     = TypeLam Kind $ ProductT [TypeBinding $ TyVar VZ]
identityTerm     = undefined


{- List -}
listTypeCtx  = insertRecType "List" listType emptyTypeCtx
listTypeName = Named "List"
listType     = TypeLam Kind $ SumT listSumType
listSumType  = [ProductT []                                        -- : List a
               {-,BigArrow Kind $                                    -- : \(x:a) (xs : List a) ->-}
                 {-Arrow (TypeBinding $ TyVar VZ) $-}
                   {-Arrow (TypeApp listTypeName (TypeBinding $ TyVar VZ)) $-}
               ,      ProductT [TypeBinding $ TyVar VZ, TypeApp listTypeName (TypeBinding $ TyVar VZ)]
               ]
emptyTerm    = BigLam Kind $ Sum (Product []) 0 listSumType
consTerm     = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) $ Lam (TypeApp listTypeName (TypeBinding $ TyVar VZ)) $ Sum (Product [Binding $ VS VZ,Binding VZ]) 1 listSumType



-- type context of bools and nats
typeCtx :: TypeCtx TyVar
typeCtx = foldr unionTypeCtx emptyTypeCtx . map fromJust $ [boolTypeCtx,natTypeCtx,listTypeCtx]


-- Boolean and
andExpr :: TestExpr
andExpr = Lam boolTypeName $ Lam boolTypeName $      -- \x:Bool y:Bool ->
    CaseAnalysis $ Case (Binding VZ)                 -- case y of
      $ CaseBranches                                 --
        ((CaseBranch falsePat falseTerm) :| []       --     False -> False
        )                                            --
        (Just                                        --     _      ->
            (CaseAnalysis $ Case (Binding $ VS VZ)   --               case x of
              $ CaseBranches                         --
                ((CaseBranch falsePat falseTerm):|[] --                 False -> False
                )                                    --
                (Just                                --                        _     ->
                    trueTerm                         --                                 True
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
subTwoExpr = Lam natTypeName $                              -- \n : Nat ->
    CaseAnalysis $ Case (Binding VZ)                        -- case n of
      $ CaseBranches                                        --
        ((CaseBranch (sPat $ sPat Bind) (Binding VZ)) :| [] --   S S n -> n
        )                                                   --
        (Just                                               --
            zTerm                                           --   _     -> Z
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
    CaseAnalysis $ Case (Binding VZ)                                 -- case x of
      $ CaseBranches                                                 --
        ((CaseBranch (MatchSum 0 $ sPat Bind) (Binding VZ))          --  0| S n   -> n
         :| [CaseBranch (MatchSum 0   zPat)      zTerm               --  0| Z     -> Z
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
    CaseAnalysis $ Case (Binding VZ)                                       -- case x of
      $ CaseBranches                                                       --
        ((CaseBranch (MatchProduct [zPat,Bind,zPat]) (Binding VZ))         -- Z,y,Z -> y
         :| [CaseBranch (MatchProduct [Bind,Bind,zPat]) (Binding VZ)]      -- x,y,Z -> y
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
    CaseAnalysis $ Case (Binding VZ)                                    -- case x of
      $ CaseBranches                                                    --
        ((CaseBranch (MatchUnion natTypeName   zPat)      falseTerm)    -- Nat | Z    -> False
         :| [CaseBranch (MatchUnion natTypeName $ sPat Bind) trueTerm   -- Nat | S n  -> True
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

-- The polymorphic identity function
idExpr :: TestExpr
idExpr = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) (Binding VZ) -- \(x:a) -> x
idExprType :: TestType
idExprType = BigArrow Kind $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding $ TyVar VZ)
idText :: Text.Text
idText = "TODO"

constExpr :: TestExpr
constExpr = BigLam Kind $ BigLam Kind $ Lam (TypeBinding $ TyVar $ VS VZ) $ Lam (TypeBinding $ TyVar VZ) $ Binding $ VS VZ -- \(x:a) (y:b) -> x
constExprType :: TestType
constExprType = BigArrow Kind $ BigArrow Kind $ Arrow (TypeBinding $ TyVar $ VS $ VZ) $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding $ TyVar $ VS VZ)

-- [0]
listNatExpr :: TestExpr
listNatExpr = (App (App (BigApp consTerm natTypeName) zero) (BigApp emptyTerm natTypeName))
listNatExprType :: TestType
listNatExprType = TypeApp listTypeName natType



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


