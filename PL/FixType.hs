{-# LANGUAGE
     ConstraintKinds
   , FlexibleContexts
   , FlexibleInstances
   , GADTs
   , GeneralizedNewtypeDeriving
   , LambdaCase
   , MultiParamTypeClasses
   , OverloadedStrings
   , RankNTypes
   , ScopedTypeVariables
   , StandaloneDeriving
   , UndecidableInstances
   , TypeSynonymInstances
   #-}
{-|
Module      : PL.FixType
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.FixType where

import PLPrinter.Doc

-- | Fix a PL type with nested sub-types 'typ'.
-- 'tb' is the type of type-bindings.
newtype FixType tb typ = FixType {_unFixType :: typ tb (FixType tb typ)}

deriving instance Show (typ tb (FixType tb typ)) => Show (FixType tb typ)
deriving instance Ord (typ tb (FixType tb typ)) => Ord (FixType tb typ)
deriving instance Eq (typ tb (FixType tb typ)) => Eq (FixType tb typ)
deriving instance Document (typ tb (FixType tb typ)) => Document (FixType tb typ)

unfixType
  :: FixType tb typ
  -> typ tb (FixType tb typ)
unfixType = _unFixType

fixType
  :: typ tb (FixType tb typ)
  -> FixType tb typ
fixType = FixType

-- | Catamorphism. Generic function fold.
cataType
  :: Functor (typ tb)
  => (typ tb typeA -> typeA)
  -> (FixType tb typ -> typeA)
cataType f = f . fmap (cataType f) . _unFixType

-- | Anamorphism. Generic function unfold.
anaType
  :: Functor (typ tb)
  => (typeA -> typ tb typeA)
  -> (typeA -> FixType tb typ)
anaType f = FixType . fmap (anaType f) . f

-- | Hylomorphism. Unfold followed by fold.
hyloType
  :: Functor (typ tb)
  => (typ tb typeB -> typeB)
  -> (typeA -> typ tb typeA)
  -> (typeA -> typeB)
hyloType phi psi = cataType phi . anaType psi

-- | Infix hylomorphism.
(~>)
  :: Functor (typ tb)
  => (typeA -> typ tb typeA)
  -> (typ tb typeB -> typeB)
  -> (typeA -> typeB)
psi ~> phi = phi . fmap (hyloType phi psi) . psi

-- | Monadic cataType.
cataTypeM
  :: ( Monad m
     , Traversable (typ tb)
     )
  => (typ tb typeA -> m typeA)
  -> FixType tb typ
  -> m typeA
cataTypeM f = (f =<<) . traverse (cataTypeM f) . _unFixType

-- | Monadic anaType.
anaTypeM
  :: ( Monad m
     , Traversable (typ tb)
     )
  => (typeA -> m (typ tb typeA))
  -> typeA
  -> m (FixType tb typ)
anaTypeM f = fmap FixType . (traverse (anaTypeM f) =<<) . f

-- | Monadic hyloType.
hyloTypeM
  :: ( Monad m
     , Traversable (typ tb)
     )
  => (typ tb typeB -> m typeB)
  -> (typeA -> m (typ tb typeA))
  -> (typeA -> m typeB)
hyloTypeM phi psi = (cataTypeM phi =<<) . anaTypeM psi
