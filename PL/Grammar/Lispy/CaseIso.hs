{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PL.Grammar.Lispy.CaseIso where

import PLGrammar
import PLGrammar.Iso

import PL.Case
import PL.Kind
import PL.TyVar
import PL.Name
import PL.Var

import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Char
import Data.List.NonEmpty

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

caseIso :: Iso (e, CaseBranches e m) (Case e m)
caseIso = Iso
  {_isoLabel = ["case"]
  ,_parseIso = \(scrutineeExpr, branches) -> Just $ Case scrutineeExpr branches
  ,_printIso = \(Case scrutineeExpr branches) -> Just (scrutineeExpr, branches)
  }

caseBranchesIso :: Iso (NonEmpty (CaseBranch e m), Maybe e) (CaseBranches e m)
caseBranchesIso = Iso
  {_isoLabel = ["caseBranches"]
  ,_parseIso = \(branches, mDefaultBranch) -> Just $ CaseBranches branches mDefaultBranch
  ,_printIso = \caseBranches -> case caseBranches of
                                  CaseBranches branches mDefaultBranch
                                    -> Just (branches, mDefaultBranch)
                                  _ -> Nothing
  }

defaultOnlyIso :: Iso e (CaseBranches e m)
defaultOnlyIso = Iso
  {_isoLabel = ["defaultOnly"]
  ,_parseIso = \resultExpr -> Just $ DefaultOnly resultExpr
  ,_printIso = \caseBranches -> case caseBranches of
                                  DefaultOnly resultExpr
                                    -> Just resultExpr
                                  _ -> Nothing
  }

caseBranchIso :: Iso (m, e) (CaseBranch e m)
caseBranchIso = Iso
  {_isoLabel = ["caseBranch"]
  ,_parseIso = \(match, result) -> Just $ CaseBranch match result
  ,_printIso = \(CaseBranch match result) -> Just (match, result)
  }
