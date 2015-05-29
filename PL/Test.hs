{-# LANGUAGE OverloadedStrings #-}
module PL.Test where

import PL.Expr
import PL.Type
import PL.Var

import qualified Data.Map as Map
import qualified Data.Set as Set

andExpr = Lam (ty "Bool") $ Lam (ty "Bool") $             -- \x:Bool y:Bool ->
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
                    (CaseTerm "False" [] $ Term "False")  --                 False -> False
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
subTwoExpr = Lam (ty "Nat") $                                   -- \n : Nat ->
    Case (Var VZ)                                               -- case n of
      $ CaseBranches                                            --
        (SomeCaseBranches                                       --
            (CaseTerm "S" [MatchTerm "S" [BindVar]] $ (Var VZ)) --   S S n -> n
            []                                                  --
        )                                                       --
        (Just                                                   --
            (Term "Z")                                          --   _     -> Z
        )

-- case analysis on a sum type with overlapping members
sum3Expr = Lam (SumT [ty "Nat",ty "Bool", ty "Nat"]) $                   -- \x : Nat|Bool|Nat ->
    Case (Var VZ)                                                        -- case x of
      $ CaseBranches
        (SomeCaseBranches
            (CaseSum 0 (MatchTerm "S" [BindVar]) (Var VZ))               --  0| S n   -> n
            [CaseSum 0 (MatchTerm "Z" []) (Term "Z")                     --  0| Z     -> Z
            ,CaseSum 1 (MatchTerm "False" []) (Term "Z")                 --  1| False -> Z
            ,CaseSum 1 (MatchTerm "True" []) (App (Term "S") (Term "Z")) --  1| True  -> S Z
            ,CaseSum 2 (MatchTerm "S" [BindVar]) (Term "Z")              --  2| S n   -> Z
            ,CaseSum 2 (MatchTerm "Z" []) (App (Term "S") (Term "Z"))    --  2| Z     -> S Z
            ]
        )
        Nothing

prod3Expr = Lam (ProdT [ty "Nat",ty "Bool",ty "Nat"]) $                     -- \x : Nat*Bool*Nat ->
    Case (Var VZ)                                                           -- case x of
      $ CaseBranches                                                        --
        (SomeCaseBranches                                                   --
            (CaseProd [MatchTerm "Z" [],BindVar,MatchTerm "Z" []] (Var VZ)) -- Z,y,Z -> y
            [CaseProd [BindVar,BindVar,MatchTerm "Z" []] (Var VZ)]          -- x,y,Z -> y
        )                                                                   --
        (Just                                                               --
            (Term "False")                                                  -- _ -> Z
        )

union2Expr = Lam (UnionT $ Set.fromList [ty "Bool",ty "Nat"]) $            -- \x : <Nat|Bool>
    Case (Var VZ)                                                          -- case x of
      $ CaseBranches                                                       --
        (SomeCaseBranches                                                  --
            (CaseUnion (ty "Nat")  (MatchTerm "Z" []) (Term "False"))      -- Nat | Z    -> False
            [CaseUnion (ty "Nat")  (MatchTerm "S" [BindVar]) (Term "True") -- Nat | S n  -> True
            ,CaseUnion (ty "Bool") (MatchTerm "True" []) (Term "True")     -- Bool| True -> True
            ]                                                              --
        )                                                                  --
        (Just                                                              --
            (Term "False")                                                 -- _          -> False
        )

runTest = mapM (topExprType nameCtx) [andExpr,subTwoExpr,sum3Expr,prod3Expr,union2Expr]


