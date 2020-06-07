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
Module      : PL.FixPhase
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.FixPhase
  ( -- Construct and deconstruct data types which recursively contain themselves
    -- and may be indexed on some phase type variable.
    FixPhase (..)
  , fixPhase
  , unfixPhase

  -- Recursion schemes over the FixPhase structure
  , (<&>)
  , cataPhase
  , anaPhase
  , hyloPhase
  , (~>)

  , cataPhaseM
  , anaPhaseM
  , hyloPhaseM

  -- Misc phase-related things
  , DefaultPhase
  , Void
  , void
  , Error
  )
  where

import PL.Hash
import PLPrinter.Doc
import PL.Error

-- | Fix a phase-indexed type with nested sub-types 'f'.
newtype FixPhase phase f
  = FixPhase {_unFixPhase :: f phase (FixPhase phase f)}

deriving instance Eq (f phase (FixPhase phase f)) => Eq (FixPhase phase f)
deriving instance Ord (f phase (FixPhase phase f)) => Ord (FixPhase phase f)
deriving instance Document (f phase (FixPhase phase f)) => Document (FixPhase phase f)

instance Show (f phase (FixPhase phase f)) => Show (FixPhase phase f) where
  show (FixPhase f) = show f

unfixPhase
  :: FixPhase phase f
  -> f phase (FixPhase phase f)
unfixPhase = _unFixPhase

fixPhase
  :: f phase (FixPhase phase f)
  -> FixPhase phase f
fixPhase = FixPhase

deriving instance Hashable (f phase (FixPhase phase f)) => Hashable (FixPhase phase f)

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}

-- | Catamorphism. Generic function fold.
cataPhase
  :: Functor (f phase)
  => (f phase f' -> f')
  -> (FixPhase phase f -> f')
cataPhase f = f . fmap (cataPhase f) . _unFixPhase

-- | Anamorphism. Generic function unfold.
anaPhase
  :: Functor (f phase)
  => (fA -> f phase fA)
  -> (fA -> FixPhase phase f)
anaPhase f = FixPhase . fmap (anaPhase f) . f

-- | Hylomorphism. Unfold followed by fold.
hyloPhase
  :: Functor (f phase)
  => (f phase fB -> fB)
  -> (fA -> f phase fA)
  -> (fA -> fB)
hyloPhase phi psi = cataPhase phi . anaPhase psi

-- | Infix hylomorphism.
(~>)
  :: Functor (f phase)
  => (fA -> f phase fA)
  -> (f phase fB -> fB)
  -> (fA -> fB)
psi ~> phi = phi . fmap (hyloPhase phi psi) . psi

-- | Monadic cataPhase.
cataPhaseM
  :: ( Monad m
     , Traversable (f phase)
     )
  => (f phase fA -> m fA)
  -> FixPhase phase f
  -> m fA
cataPhaseM f = (f =<<) . traverse (cataPhaseM f) . _unFixPhase

-- | Monadic anaPhase.
anaPhaseM
  :: ( Monad m
     , Traversable (f phase)
     )
  => (fA -> m (f phase fA))
  -> fA
  -> m (FixPhase phase f)
anaPhaseM f = fmap FixPhase . (traverse (anaPhaseM f) =<<) . f

-- | Monadic hyloPhase.
hyloPhaseM
  :: ( Monad m
     , Traversable (f phase)
     )
  => (f phase fB -> m fB)
  -> (fA -> m (f phase fA))
  -> (fA -> m fB)
hyloPhaseM phi psi = (cataPhaseM phi =<<) . anaPhaseM psi

data DefaultPhase

data Void

-- | Errors that occur in the DefaultPhase
type Error = ErrorFor DefaultPhase

instance Show Void where
  show _ = ""

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

instance Hashable Void where
  toHashToken _ = HashTag "" []

-- Some patterns to make working with ExprF nicer
void :: Void
void = error "Cannot evaluate Void"

