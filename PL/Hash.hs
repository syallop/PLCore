{-# LANGUAGE
    FlexibleInstances
  , OverloadedStrings
  #-}
{-|
Module      : PL.Hash
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Hashes uniquely identify a 'hash'ed thing provided:
- You're not unfortunate enough to see a hash-collision.
- The thing has a 'Hashable' instance which declares how 'toHashToken'
  transforms it to a 'HashToken' in such a way that all unique inputs map to
  unique 'HashToken's.

'Hashable' instances should:
- Use 'HashText', 'HashInt', etc for plain primitives.
- Use 'HashIs' to embed already known hashes.
- Use HashTag to encode unique data types or constructors. E.G.

data T = A
       | B Int Text
       | C Hash

Might be encoded:

A       = HashTag "T.A" []
(B i t) = HashTag "T.B" [HashInt i, HashText t]
(C h)   = HashTag "T.C" [HashIs h]

-}
module PL.Hash
  ( -- Hashes uniquely identify a hashed thing.
    Hash ()
  , HashAlgorithm (..)
  , hash
  , hashWith
  , showBase58
  , readBase58
  , hashAlgorithm
  , hashBytes

  -- In order to be hashed, a type must have a Hashable instance which declares
  -- how toHashToken transforms it into a HashToken.
  , HashToken (..)
  , Hashable
  , toHashToken
  )
  where

import Data.ByteString.Builder
import Data.ByteString.UTF8 as BSU (fromString)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Encoding
import qualified Crypto.Hash as CH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text

-- For Hashable instances
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Hash uniquely identifies a 'hash'ed thing.
data Hash = Hash
  { _hashAlgorithm :: HashAlgorithm -- ^ By storing the algorithm used to generate the hash we can more easily regenerate/ and switch the algorithm if necessary.
  , _unHash        :: BS.ByteString -- ^ The hashed bytes. Not necessarily human readable.
  }
  deriving (Eq,Ord)

-- | By default hashes are shown in a full base58 encoding.
instance Show Hash where
  show = Text.unpack . showBase58

-- | Get a unique Hash of a Hashable thing using the default SHA512 algorithm.
hash :: Hashable h => h -> Hash
hash = hashWith SHA512

-- | Get a unique Hash of a Hashable thing with a particular algorithm.
hashWith
  :: Hashable h
  => HashAlgorithm
  -> h
  -> Hash
hashWith alg = hashToken alg . toHashToken

-- | Render a Hash as human-readable text with it's algorithm identifier
-- separated by a '/' and followed by a base58 interpretation of the hash
-- itself.
showBase58 :: Hash -> Text
showBase58 (Hash alg h) = mconcat
  [ hashIdentifier alg
  , "/"
  , decodeUtf8 . B58.encodeBase58 B58.bitcoinAlphabet $ h
  ]

-- | Attempt to read a Base58 textual representation of a Hash with a preceeding
-- algorithm identifier, separated by a '/'.
readBase58 :: Text -> Maybe Hash
readBase58 txt = do
  case Text.split (== '/') txt of
    (algText:hashText:[])
      -> case readHashAlgorithm algText of
           Just SHA512
             -> do bytes <- B58.decodeBase58 B58.bitcoinAlphabet . encodeUtf8 $ hashText
                   if BS.length bytes /= 128
                     then Nothing
                     else Just $ Hash SHA512 bytes
           _
             -> error "Unrecognised hashing algorithm"
    _
      -> Nothing

-- | Retrieve the algorithm used to generate a Hash.
hashAlgorithm
  :: Hash
  -> HashAlgorithm
hashAlgorithm (Hash a _) = a

-- | Extract the bytes component of the hash _without_ the algorithm and without
-- encoding to any human readable form.
hashBytes
  :: Hash
  -> BS.ByteString
hashBytes (Hash _a bytes) = bytes

-- | An algorithm capable of hashing HashTokens to a Hash.
-- This enumeration may be extended with new known hashes but hashes identifiers
-- should remain static.
data HashAlgorithm
  = SHA512
  deriving (Eq,Ord)

-- | Uniquely identify a hash algorithm with human readable text.
hashIdentifier :: HashAlgorithm -> Text
hashIdentifier a = case a of
  SHA512
    -> "SHA512"
  _ -> error "Hash Algorithm unknown"

-- | Read a hash identifier to the corresponding algorithm.
readHashAlgorithm :: Text -> Maybe HashAlgorithm
readHashAlgorithm txt = case txt of
  "SHA512"
    -> Just SHA512
  _ -> Nothing

-- | Lookup a HashAlgorithm that transforms opaque bytes to hashed bytes.
hashFunction :: HashAlgorithm -> BL.ByteString -> BS.ByteString
hashFunction a = case a of
  SHA512
    -> BSU.fromString . show . (CH.hashLazy :: BL.ByteString -> CH.SHA512)

-- | HashToken is a complete AST of a 'Hashable' thing.
-- All unique values should have a unique HashToken representation to ensure
-- hashes are distinct for distict values.
--
-- Convert to a compact 'Hash' with 'hash'.
--
-- When constructing:
-- - Use 'HashText', 'HashInt', etc for plain primitives.
-- - Use 'HashIs' to embed already known hashes. These will not be rehashed.
-- - Use HashTag to encode unique data types or constructors. E.G.
--
-- data T = A
--        | B Int Text
--       | C Hash
--
-- Might be encoded:
--
-- A       = HashTag "T.A" []
-- (B i t) = HashTag "T.B" [HashInt i, HashText t]
-- (C h)   = HashTag "T.C" [HashIs h]
--
data HashToken
  = HashTag Text [HashToken]
  | HashText Text
  | HashInt Int
  | HashIs Hash

-- Realize a HashToken to it's compact Hash using the provided algorithm.
--
-- HashIs tokens should be unmodified meaning values should be able to be
-- substituted for their hash without changing the output.
hashToken
  :: HashAlgorithm
  -> HashToken
  -> Hash
hashToken alg h = case h of
  HashInt i
    -> Hash alg . hashFunction alg . toLazyByteString . mconcat $
         [ byteString "int"
         , int64BE . fromIntegral $ i
         ]

  HashText txt
    -> Hash alg . hashFunction alg . toLazyByteString . mconcat $
         [ byteString "text"
         , byteString . encodeUtf8 $ txt
         ]

  HashTag tagTxt args
    -> let hashedArgs :: [Hash]
           hashedArgs = fmap (hashToken alg) args

           bsArgs :: [BS.ByteString]
           bsArgs = fmap _unHash hashedArgs
        in Hash alg . hashFunction alg . toLazyByteString . mconcat $
             [ byteString "tag"
             , byteString . encodeUtf8 $ tagTxt
             , int64BE . fromIntegral . length $ args
             , byteString "["
             , mconcat . fmap byteString . intersperse "," $ bsArgs
             , byteString "]"
             ]

  HashIs (Hash alg bs)
    -> Hash alg bs

class Hashable h where
  toHashToken :: h -> HashToken

instance Hashable HashToken where
  toHashToken = id

instance Hashable Hash where
  toHashToken = HashIs

instance Hashable Text where
  toHashToken = HashText

instance Hashable Int where
  toHashToken = HashInt

instance Hashable h => Hashable (Text,[h]) where
  toHashToken (tag,args) = HashTag tag (fmap toHashToken args)

instance Hashable h => Hashable [h] where
  toHashToken hs = HashTag "[]" (fmap toHashToken hs)

instance Hashable h => Hashable (NonEmpty h) where
  toHashToken hs = HashTag "NonEmpty" (fmap toHashToken . NE.toList $ hs)

instance Hashable h => Hashable (Set h) where
  toHashToken hs = HashTag "Set" (fmap toHashToken . Set.toAscList $ hs)

