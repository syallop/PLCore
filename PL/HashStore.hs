{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , RankNTypes
           , GADTs
           , LambdaCase
           , ConstraintKinds
           , OverloadedStrings
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
  , HashBackingStorage
  , newHashStore
  , storeByHash
  , lookupByHash

  , ShortHash ()
  , largerHashes
  , shortenHash
  )
  where

import Prelude hiding (lookup)

import Data.Text

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as B58
import Data.Text.Encoding
import qualified Data.Text as Text

import PL.Store
import PL.ShortStore
import PL.Hash

-- | Store values 'v' by their 'Hash' in some underlying 'Store'.
data HashStore v = forall s. (ShortStore s Hash ShortHash v, Hashable v, Show (s Hash v)) => HashStore
  { _store :: s Hash v
  }

instance Show (HashStore v) where
  show h = case h of
    HashStore s
      -> show s

type HashBackingStorage s v = ShortStore s Hash ShortHash v

-- | Create a HashStore from some underlying backing Storage which accepts
-- 'Hash'es as keys.
newHashStore :: (Hashable v, HashBackingStorage s v, Show (s Hash v)) => s Hash v -> HashStore v
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

-- A Hash that may have had it's bytes truncated an arbitrary amount.
data ShortHash = ShortHash
  { _shortHashAlgorithm :: HashAlgorithm
  , _unShortHash        :: BS.ByteString
  }
  deriving (Eq, Ord)

instance Show ShortHash where
  show (ShortHash alg h) = Text.unpack . mconcat $
    [ hashIdentifier alg
    , "/"
    , decodeUtf8 . B58.encodeBase58 B58.bitcoinAlphabet $ h
    ]

instance Shortable Hash ShortHash where
  shortLength = shortLength . _unShortHash

  toShort h = ShortHash (hashAlgorithm h) (hashBytes h)

  shortenAgainst sourceHash againstHash
    -- If the hashes use a different algorithm the bytes arent needed to
    -- identify them.
    | hashAlgorithm sourceHash /= hashAlgorithm againstHash
     = ShortHash (hashAlgorithm sourceHash) ""

    -- If the hashes use the same algorithm, find the shortest sequence of
    -- unique bytes.
    | otherwise
     = let bs = shortenAgainst (hashBytes sourceHash) (hashBytes againstHash)
        in ShortHash (hashAlgorithm sourceHash) bs

-- | Given a ShortHash, return all known larger Hashes
largerHashes
  :: HashStore v
  -> ShortHash
  -> IO (Maybe (HashStore v, [Hash]))
largerHashes (HashStore s) shortHash = fmap (\(s',hashes) -> (HashStore s',hashes)) <$> largerKeys s shortHash

-- | Given a regular Hash, return the shortest unambiguous Hash.
shortenHash
  :: HashStore v
  -> Hash
  -> IO (Maybe (HashStore v, ShortHash))
shortenHash (HashStore s) hash = fmap (\(s',shortHash) -> (HashStore s',shortHash)) <$> shortenKey s hash

