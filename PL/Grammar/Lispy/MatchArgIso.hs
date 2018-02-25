{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PL.Grammar.Lispy.MatchArgIso where

import PLGrammar
import PLGrammar.Iso

import PL.Case
import PL.Expr
import PL.Type
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

matchSumIso :: Iso (Int, MatchArg b tb) (MatchArg b tb)
matchSumIso = Iso
  {_isoLabel = ["matchSum"]
  ,_parseIso = \(ix, matchArg) -> Just $ MatchSum ix matchArg
  ,_printIso = \matchArg -> case matchArg of
                              MatchSum ix matchArg
                                -> Just (ix, matchArg)
                              _ -> Nothing
  }

matchProductIso :: Iso [MatchArg b tb] (MatchArg b tb)
matchProductIso = Iso
  {_isoLabel = ["matchProduct"]
  ,_parseIso = \matchArgs -> Just $ MatchProduct matchArgs
  ,_printIso = \matchArg -> case matchArg of
                              MatchProduct matchArgs
                                -> Just matchArgs
                              _ -> Nothing
  }

matchUnionIso :: Iso (Type tb, MatchArg b tb) (MatchArg b tb)
matchUnionIso = Iso
  {_isoLabel = ["matchUnion"]
  ,_parseIso = \(tyIx, matchArg) -> Just $ MatchUnion tyIx matchArg
  ,_printIso = \matchArg -> case matchArg of
                              MatchUnion tyIx matchArg
                                -> Just (tyIx, matchArg)
                              _ -> Nothing
  }

matchBindingIso :: Iso b (MatchArg b tb)
matchBindingIso = Iso
  {_isoLabel = ["matchBinding"]
  ,_parseIso = \b -> Just $ MatchBinding b
  ,_printIso = \matchArg -> case matchArg of
                              MatchBinding b
                                -> Just b
                              _ -> Nothing
  }

matchBindIso :: Iso () (MatchArg b tb)
matchBindIso = Iso
  {_isoLabel = ["matchBind"]
  ,_parseIso = \() -> Just Bind
  ,_printIso = \matchArg -> case matchArg of
                              Bind
                                -> Just ()
                              _ -> Nothing
  }

