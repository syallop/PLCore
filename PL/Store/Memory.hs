{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , RankNTypes
           , TupleSections
           , UndecidableInstances
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

  , largerKeysInMemory
  , shortenKeyInMemory
  )
  where

import Prelude hiding (lookup)

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.ByteString (ByteString)

import PL.Store
import PL.ShortStore

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

instance
  ( Ord k
  , Ord v
  ) => Store MemoryStore k v where
  store s k v = pure $ storeInMemory s k v
  lookup s k  = pure $ lookupFromMemory s k

instance
  ( Ord k
  , Ord v
  , Ord shortK
  , Shortable k shortK
  ) => ShortStore MemoryStore k shortK v where
  largerKeys s shortK = pure . Just $ (s, largerKeysInMemory s shortK)
  shortenKey s k      = pure . fmap (s,) . shortenKeyInMemory s $ k

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

-- | Lookup all known keys that are 'larger' than a short key.
largerKeysInMemory
  :: ( Ord shortK
     , Shortable k shortK
     )
  => MemoryStore k v
  -> shortK
  -> [k]
largerKeysInMemory (MemoryStore s) shortKey = filter (\key -> shortKey <= toShort key) . Map.keys $ s
-- TODO: If we maintained a Trie we may be able to perform this operation
-- more efficiently.
-- Alternatively, Maps can be queried using the Ord instance like lookupGE. If
-- shortkeys could be converted to keys for the purpose of this comparison we
-- could use this instead of going the other way around.

-- | Shorten a key to the smallest possible unambiguous ShortKey.
shortenKeyInMemory
  :: Shortable k shortK
  => MemoryStore k v
  -> k
  -> Maybe shortK
shortenKeyInMemory (MemoryStore s) key = case fmap (shortenAgainst key . Just) . Map.keys $ s of
  -- No keys, shorten as much as possible
  []
    -> Just . shortenAgainst key $ Nothing

  xs
    -> Just . head . List.sortOn shortLength $ xs
-- TODO: We could perform this operation much more efficiently with a better
-- datastructure.

