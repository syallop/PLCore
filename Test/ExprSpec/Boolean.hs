{-# LANGUAGE OverloadedStrings #-}
module ExprSpec.Boolean
  ( boolTypeCtx
  , boolTypeName
  , boolType
  , boolSumType
  , falseTerm
  , trueTerm
  , falsePat
  , truePat
  , falseTermText
  , trueTermText
  , falsePatText
  , truePatText

  , andExpr
  , andExprType
  , andText
  )
  where

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

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))

type TestType = Type TyVar
type TestExpr = Expr Var TestType TyVar

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

falseTermText, trueTermText, falsePatText, truePatText :: Text

falseTermText = "+0(*) (*) (*)"
trueTermText  = "+1(*) (*) (*)"
falsePatText  = "+0(*)"
truePatText   = "+1(*)"

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
andText :: Text
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

