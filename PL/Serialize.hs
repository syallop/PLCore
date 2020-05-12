{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RankNTypes
  #-}
{-|
Module      : PL.Serialize
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Class of types which can be Serialized to ByteStrings and back.
The format should be suitable for writing to disk/ over a network and need not
be human readable.

-}

module PL.Serialize
  ( Serialize
  , serialize
  , deserialize
  )
  where

import Data.Text
import Data.ByteString
import Data.Text.Encoding

class Serialize t where
  serialize   :: t -> ByteString
  deserialize :: ByteString -> Maybe t

instance Serialize Text where
  serialize   = encodeUtf8
  deserialize = either (const Nothing) Just . decodeUtf8'

