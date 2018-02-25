{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PL.Grammar.Lispy.KindIso where

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

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

kindIso :: Iso () Kind
kindIso = Iso
  {_isoLabel = ["kind"]
  ,_parseIso = \() -> Just Kind
  ,_printIso = \Kind -> Just ()
  }

kindArrowIso :: Iso (Kind,Kind) Kind
kindArrowIso = Iso
  {_isoLabel = ["kindArrow"]
  ,_parseIso = \(fromKy, toKy) -> Just $ KindArrow fromKy toKy
  ,_printIso = \kind -> case kind of
                          KindArrow fromKy toKy
                            -> Just (fromKy, toKy)

                          _ -> Nothing
  }

