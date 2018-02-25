{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PL.Grammar.Lispy.TypeIso where

import PLGrammar
import PLGrammar.Iso

import PL.Grammar.Lispy.Kind

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.TyVar
import PL.Name
import PL.Var

import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Char

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

-- A Name is text with an upper case first character and lowercase remaining
-- letters.
-- TODO: Doesnt belong here.
nameIso :: Iso (Char, Text.Text) Text.Text
nameIso = Iso
  {_parseIso = \(c,cs) -> if
               | isUpper c && Text.all isLower cs -> Just $ Text.cons c cs
               | otherwise -> Nothing
  ,_printIso = \txt -> case Text.uncons txt of
                         Nothing
                           -> Nothing

                         Just (c,cs)
                           | isUpper c && Text.all isLower cs -> Just (c,cs)
                           | otherwise -> Nothing
  }

-- TODO: Doesnt belong here
typeNameIso :: Iso Text.Text TypeName
typeNameIso = Iso
  {_parseIso = \txt -> Just $ TypeName txt
  ,_printIso = \(TypeName txt) -> Just txt
  }

namedIso :: Iso TypeName (Type tb)
namedIso = Iso
  {_parseIso = \typeName
                -> Just $ Named typeName
  ,_printIso = \ty
                -> case ty of
                     Named typeName
                       -> Just typeName
                     _ -> Nothing
  }

arrowIso :: Iso (Type tb, Type tb) (Type tb)
arrowIso = Iso
  {_parseIso = \(fromTy, toTy)
                -> Just $ Arrow fromTy toTy
  ,_printIso = \ty
                -> case ty of
                     Arrow fromTy toTy
                       -> Just (fromTy, toTy)
                     _ -> Nothing
  }

sumTIso :: Iso [Type tb] (Type tb)
sumTIso = Iso
  {_parseIso = \tys
                -> Just $ SumT tys
  ,_printIso = \ty
                -> case ty of
                     SumT tys
                       -> Just tys
                     _ -> Nothing
  }

productTIso :: Iso [Type tb] (Type tb)
productTIso = Iso
  {_parseIso = \tys
                -> Just $ ProductT tys
  ,_printIso = \ty
                -> case ty of
                     ProductT tys
                       -> Just tys
                     _ -> Nothing
  }

unionTIso :: Iso (Set.Set (Type tb)) (Type tb)
unionTIso = Iso
  {_parseIso = \tys
                -> Just $ UnionT tys
  ,_printIso = \ty
                -> case ty of
                     UnionT tys
                       -> Just tys
                     _ -> Nothing
  }

bigArrowIso :: Iso (Kind, Type tb) (Type tb)
bigArrowIso = Iso
  {_parseIso = \(fromKind, toTy)
                -> Just $ BigArrow fromKind toTy
  ,_printIso = \ty
                -> case ty of
                    BigArrow fromKind toTy
                      -> Just (fromKind, toTy)
                    _ -> Nothing
  }

typeLamIso :: Iso (Kind, Type tb) (Type tb)
typeLamIso = Iso
  {_parseIso = \(fromKind, toTy)
                -> Just $ TypeLam fromKind toTy
  ,_printIso = \ty
                -> case ty of
                    TypeLam fromKind toTy
                      -> Just (fromKind, toTy)
                    _ -> Nothing
  }


typeAppIso :: Iso (Type tb, Type tb) (Type tb)
typeAppIso = Iso
  {_parseIso = \(fTy, xTy)
                -> Just $ TypeApp fTy xTy
  ,_printIso = \ty
                -> case ty of
                     TypeApp fTy xTy
                       -> Just (fTy, xTy)
                     _ -> Nothing
  }

typeBindingIso :: Iso tb (Type tb)
typeBindingIso = Iso
  {_parseIso = \tb
                -> Just $ TypeBinding tb
  ,_printIso = \ty
                -> case ty of
                     TypeBinding tb
                       -> Just tb
                     _ -> Nothing
  }

-- TODO: Doesnt belong here.
-- TODO: Partial.
tyVarIso :: Iso Int TyVar
tyVarIso = Iso
  {_parseIso = Just . mkTyVar
  ,_printIso = \(TyVar v) -> Just . fromEnum $ v
  }

-- TODO: Doesnt belong here.
setIso :: Ord a => Iso [a] (Set.Set a)
setIso = Iso
  {_parseIso = Just . Set.fromList
  ,_printIso = Just . Set.toList
  }

