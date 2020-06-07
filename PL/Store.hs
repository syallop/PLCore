{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RankNTypes
           , TypeFamilies
           , StandaloneDeriving
           , FunctionalDependencies
  #-}
{-|
Module      : PL.Store
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Store is an interface to key-value storage and lookup where:

- Lookup may mutate the container.
- Storage may report that it has replaced values.
-}
module PL.Store
  ( Store ()
  , StoreResult (..)

  -- Regular store/ lookup api
  , store
  , lookup
  )
  where

import Prelude hiding (lookup)

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid

import PL.Hash
import PL.Error
import qualified Data.ByteString as BS

-- | The result of successfully storing something is a StoreResult.
data StoreResult v
  = Successfully -- ^ A Successful store of a value that has not been seen before.
  | Overwritten  -- ^ A Successful replacement of value(s) with a new value.
     { _oldValue :: Set v
     }
  | AlreadyStored -- No action as the value is already stored.
  deriving Show

instance Ord v => Monoid (StoreResult v) where
  mempty   = Successfully

instance Ord v => Semigroup (StoreResult v) where
  s0 <> s1 = case (s0, s1) of
    (Overwritten vs0, Overwritten vs1)
      -> Overwritten (vs0 <> vs1)
    (Overwritten vs, _)
      -> Overwritten vs
    (_,Overwritten vs)
      -> Overwritten vs

    (AlreadyStored,AlreadyStored)
      -> AlreadyStored
    (AlreadyStored,_)
      -> AlreadyStored
    (_,AlreadyStored)
      -> AlreadyStored

    (Successfully,Successfully)
      -> Successfully

-- | Class of store types 's' which can be used to store values 'v' associated
-- to keys 'k'.
--
-- - Lookup may mutate the container.
-- - Storage may report that it has replaced values.
class Store s k v where
  -- TODO: Perhaps move IO, Maybe, under a 'm' type parameter.

  -- | In a storage container 's', associate a value 'v' with key 'k'.
  -- If successful, report a potentially updated container and they type of
  -- success (whether the value is fresh or replaced an old value, etc).
  --
  -- Callers should use the returned storage container if they expect to be able
  -- to retrieve the stored value. No guarantees are made about the initial
  -- value and whether it makes sense to reuse.
  store
    :: s k v
    -> k
    -> v
    -> IO (Either (ErrorFor phase) (s k v, StoreResult v))

  -- | Lookup a value 'v' by it's key 'k' in the storage container 's'.
  -- An updated container is returned in the success case to allow the contain
  -- to be mutated, for example to rebalance internal structures or adjust
  -- caches.
  lookup
    :: s k v
    -> k
    -> IO (Either (ErrorFor phase) (s k v, Maybe v))

