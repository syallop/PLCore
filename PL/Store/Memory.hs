{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RankNTypes
  #-}
{-|
Module      : PL.Store.Memory
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

In-memory implementation of PL.Store

-}
module PL.Store.Memory
  ( MemoryStore ()
  , newEmptyMemoryStore
  , newMemoryStore
  , storeInMemory
  , lookupFromMemory
  )
  where

import Prelude hiding (lookup)

import Data.Text

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import PL.Store

-- | Store key-values in an in-memory Map.
data MemoryStore k v = MemoryStore
  { _memoryStore :: Map k v
  }
  deriving Show

-- | Create an empty memory store.
newEmptyMemoryStore :: Ord k => MemoryStore k v
newEmptyMemoryStore = MemoryStore mempty

-- | Create a memory store from a Map.
newMemoryStore :: Map k v -> MemoryStore k v
newMemoryStore m = MemoryStore m

instance (Ord k, Ord v) => Store MemoryStore k v where
  store  = storeInMemory
  lookup = lookupFromMemory

-- | Store a key-value association in memory.
storeInMemory
  :: ( Ord k
     , Ord v
     , Eq v
     )
  => MemoryStore k v
  -> k
  -> v
  -> Maybe (MemoryStore k v, StoreResult v)
storeInMemory (MemoryStore s) key value =
    let (mOld, s') = Map.insertLookupWithKey (\_key _old new -> new) key value s
     in Just $ case mOld of
          Nothing
            -> (MemoryStore s', Successfully)

          Just old
            | old == value
             -> (MemoryStore s', AlreadyStored)

            | otherwise
             -> (MemoryStore s', Overwritten $ Set.fromList [old])

-- | Retrieve a key-value association from memory.
lookupFromMemory
  :: Ord k
  => MemoryStore k v
  -> k
  -> Maybe (MemoryStore k v, v)
lookupFromMemory (MemoryStore s) key = case Map.lookup key s of
    Nothing
      -> Nothing

    Just value
      -> Just (MemoryStore s, value)

