{-# LANGUAGE OverloadedStrings #-}
module PL.Test where

import PL.Expr
import PL.Type
import PL.Var

import qualified Data.Map as Map

andExpr = Lam (ty "Bool") $ Lam (ty "Bool") $             -- \x y ->
    Case (Var VZ)                                         -- case y of
      $ CaseBranches                                      --
        (SomeCaseBranches                                 --
            (CaseTerm "False" [] $ Term "False")         --    False -> False
            []                                            --
        )                                                 --
        (Just                                             --    _      ->
            (Case (Var $ VS VZ)                           --              case x of
              $ CaseBranches                              --
                (SomeCaseBranches                         --
                    (CaseTerm "False" [] $ Term "False") --                 False -> False
                    []                                    --
                )                                         --
                (Just                                     --                 _     ->
                    (Term "True")                         --                          True
                )
            )
        )

boolNameCtx :: NameCtx
boolNameCtx = Map.fromList
    [("True" , TermInfo [] "Bool")
    ,("False", TermInfo [] "Bool")
    ]

natNameCtx :: NameCtx
natNameCtx = Map.fromList
    [("Z", TermInfo [] "Nat")
    ,("S", TermInfo [ty "Nat"] "Nat")
    ]

nameCtx :: NameCtx
nameCtx = boolNameCtx `Map.union` natNameCtx

-- test nested pattern matching
-- n > 2 ~> n-2
-- otherwise ~> 0
subTwoExpr = Lam (ty "Nat") $
    Case (Var VZ)
      $ CaseBranches
        (SomeCaseBranches
            (CaseTerm "S" [MatchTerm "S" [BindVar]] $ (Var VZ))
            []
        )
        (Just
            (Term "Z")
        )

-- case analysis on a sum type with overlapping members
sum3Expr = Lam (SumT [ty "Nat",ty "Bool", ty "Nat"]) $
    Case (Var VZ)
      $ CaseBranches
        (SomeCaseBranches
            (CaseSum 0 (MatchTerm "S" [BindVar]) (Var VZ))
            [CaseSum 0 (MatchTerm "Z" []) (Term "Z")
            ,CaseSum 1 (MatchTerm "False" []) (Term "Z")
            ,CaseSum 1 (MatchTerm "True" []) (App (Term "S") (Term "Z"))
            ,CaseSum 2 (MatchTerm "S" [BindVar]) (Term "Z")
            ,CaseSum 2 (MatchTerm "Z" []) (App (Term "S") (Term "Z"))
            ]
        )
        Nothing

runTest = mapM (topExpType nameCtx) [andExpr,subTwoExpr,sum3Expr]


