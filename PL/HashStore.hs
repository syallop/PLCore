{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , RankNTypes
           , GADTs
           , LambdaCase
  #-}
{-|
Module      : PL.HashStore
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

A HashStore is a data structure that stores and retrieves values by their Hash.

It is constructed with the more general Store for backing storage and handles:
- Generating keys as Hashes of values on storage operations
- Verifying the contents of values on lookup
-}
module PL.HashStore
  ( HashStore ()
  , newHashStore
  , storeByHash
  , lookupByHash
  )
  where

import Prelude hiding (lookup)

import Data.Text

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import PL.Store
import PL.Hash

-- | Store values 'v' by their 'Hash' in some underlying 'Store'.
data HashStore v = forall s. (Store s Hash v, Hashable v, Show (s Hash v)) => HashStore
  { _store :: s Hash v
  }

instance Show (HashStore v) where
  show h = case h of
    HashStore s
      -> show s

-- | Create a HashStore from some underlying backing Storage which accepts
-- 'Hash'es as keys.
newHashStore :: (Hashable v, Store s Hash v, Show (s Hash v)) => s Hash v -> HashStore v
newHashStore s = HashStore s

-- | Store a value 'v' in the 'HashStore' using the provided algorithm to
-- generate the Hash that will be used as the key.
--
-- Returns a potentially updated HashStore, a description of the success and the
-- key that can be used to retrieve the original value.
--
-- E.G.
-- h :: HashStore Text
-- h = newHashStore . newNestedStore newEmptyMemoryStore . newFileStore ".store" $ Just 32
--
-- Constructs a HashStore that:
-- - Caches in-memory
-- - Stores values in files under `.store`, using a 32 character prefix for subdirectories
storeByHash
  :: HashStore v
  -> HashAlgorithm
  -> v
  -> IO (Maybe (HashStore v, StoreResult v, Hash))
storeByHash h alg v = case h of
  HashStore s
    -> do let hashKey = hashWith alg v
          mResult <- store s hashKey v
          case mResult of
            Nothing
              -> pure Nothing

            Just (s', res)
              -> pure $ Just $ (HashStore s', res, hashKey)

-- | Retrieve a store value from a HashStore using it's Hash as a key.
lookupByHash
  :: HashStore v
  -> Hash
  -> IO (Maybe (HashStore v, v))
lookupByHash h hashKey = case h of
  HashStore s
    -> lookup s hashKey >>= \case
         Nothing
           -> pure Nothing

         Just (s', value)
           -> let valueHash = hashWith (hashAlgorithm hashKey) value
               in if valueHash /= hashKey
                    then error $ mconcat [ "HashStore returned an incorrect value for a hash. Asked for hash \n"
                                         , show hashKey
                                         , "\n but returned value hashed to \n"
                                         , show valueHash
                                         ]
                    else pure $ Just (HashStore s', value)

