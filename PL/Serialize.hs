{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RankNTypes
           , OverloadedStrings
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

import PL.Hash
import PL.Error

import PLPrinter.Doc

class Serialize t where
  serialize   :: t -> ByteString
  deserialize :: forall phase. ByteString -> Either (ErrorFor phase) t

instance Serialize Text where
  serialize   = encodeUtf8
  deserialize = either (\unicodeErr
                         -> Left . EMsg . mconcat $
                                 [ text "Failed to deserialize text with unicode error:"
                                 , lineBreak
                                 , string . show $ unicodeErr
                                 ]
                       )
                       Right

              . decodeUtf8'

instance Serialize Hash where
  serialize   = serialize . showBase58
  deserialize = (readBase58 =<<) . deserialize

