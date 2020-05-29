{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , OverloadedStrings
  #-}
module PL.ShortStore
  (
    Shortable ()
  , shortenAgainst
  , shortLength
  , toShort

  , ShortStore ()
  , largerKeys
  , shortenKey

  , ShortHash (..)
  )
  where

import PL.Store
import PL.Hash
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import qualified Data.ByteString as BS

-- A ShortStore is a Store which understands how to deal with shortened keys.
class (Store s k v, Shortable k shortK) => ShortStore s k shortK v where
  -- | Given a ShortKey, return all known larger keys.
  largerKeys
    :: s k v
    -> shortK
    -> IO (Maybe (s k v, [k]))

  -- | Given a regualar key, return the shortest unambiguous key.
  shortenKey
    :: s k v
    -> k
    -> IO (Maybe (s k v, shortK))

-- Shortable maps a long type to a shorter version.
class Shortable long short | long -> short, short -> long where
  -- Longest shared prefix of two keys
  -- Shortest unambiguous key against a second key.
  -- E.G.
  -- aaaabcd
  -- aaaaxyz
  -- = aaaab

  -- Shorten something against another. E.G.:
  -- abcDEF
  -- abXYX
  -- =
  -- abc
  shortenAgainst :: long -> long -> short

  -- For comparing the length of shorts to each other
  shortLength :: short -> Int

  -- Coerce a long thing into a short thing. Not guaranteed to actually make the
  -- thing shorter.
  toShort :: long -> short

-- A Hash that may have had it's bytes truncated an arbitrary amount.
data ShortHash = ShortHash
  { _shortHashAlgorithm :: HashAlgorithm
  , _unShortHash        :: BS.ByteString
  }

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

instance Shortable BS.ByteString BS.ByteString where
  shortLength = BS.length

  toShort bs = bs

  shortenAgainst sourceBs againstBs =
    let (common, uncommon) = span (\(a,b) -> a == b) $ BS.zip sourceBs againstBs
     in BS.pack . fmap fst $ case uncommon of
          -- Two bytestrings are the same.
          []
            -> common

          (u:_)
            -> common ++ [u]

instance Shortable Text Text where
  shortLength = shortLength . encodeUtf8

  toShort = decodeUtf8 . toShort . encodeUtf8

  shortenAgainst sourceBs againstBs = decodeUtf8 $ shortenAgainst (encodeUtf8 sourceBs) (encodeUtf8 againstBs)

