{-# LANGUAGE OverloadedStrings #-}
module PL.Test where

import Expr
import Type
import Var

import qualified Data.Map as Map

andExpr = Lam (ty "Bool") $ Lam (ty "Bool") $             -- \x y ->
    Case (Var VZ)                                         -- case y of
      $ CaseBranches                                      --
        (SomeCaseBranches                                 --
            (MatchTerm "False" [] $ Term "False")         --    False -> False
            []                                            --
        )                                                 --
        (Just                                             --    _      ->
            (Case (Var $ VS VZ)                           --              case x of
              $ CaseBranches                              --
                (SomeCaseBranches                         --
                    (MatchTerm "False" [] $ Term "False") --                 False -> False
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
            (MatchTerm "S" [MatchLit "S" [BindVar]] $ (Var VZ))
            []
        )
        (Just
            (Term "Z")
        )

