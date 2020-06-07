{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module PL.Test.Store.File.PathSpec where

import Test.Hspec

-- Module under test
import PL.Store.File.Path

import PL.Hash
import PL.Error
import PL.Var
import PL.Type
import PL.Expr
import PL.FixPhase

import PLGrammar
import Reversible
import Reversible.Iso

import Data.Text (Text)
import qualified Data.Text as Text
import PLPrinter.Doc

data GeneratePathTestCase = GeneratePathTestCase
  { _generateTestName :: Text
  , _hashBytesBase58  :: Text
  , _generatePattern  :: PathPattern Hash
  , _expectPath       :: Either Error FilePath
  }

testGeneration
  :: GeneratePathTestCase
  -> Spec
testGeneration g = describe (Text.unpack . _generateTestName $ g) $ do
  -- We must be able to generate a hash from the base58 encoded bytes, assuming
  -- SHA512 as the algorithm

  it "Generates the expected path" $ do
    case mkBase58 SHA512 . _hashBytesBase58 $ g of
      Left (err :: Error)
        -> fail . show $ err

      Right h
        -> generatePath h (_generatePattern g) `shouldBe` (_expectPath g)

data ReadPathTestCase = ReadPathTestCase
  { _readTestName :: Text
  , _path         :: Text
  , _readPattern  :: PathPattern Hash
  , _expectParse  :: (Text, Maybe Hash)
  }

testReader
  :: ReadPathTestCase
  -> Spec
testReader r = describe (Text.unpack . _readTestName $ r) $ do
  it "Reads keys from paths as expected" $
    readPathKey (_path r) (_readPattern r)
      `shouldBe`
      (_expectParse r)

-- Manually test a single example generates the expected path and then reads
-- back.
spec :: Spec
spec = do
  -- TODO:
  -- - Encapsulate roundtrip property we're manually testing
  -- - Negative tests that we don't match unexpected files, things fail when
  --   malformed etc.

  describe "File path patterns" $ do
    describe "Generation" $ do
      testGeneration $ GeneratePathTestCase
                         "Single example"
                         "BAt7JLYgp8oAyt15RSTiA6QQGW3nHjU8X7cUEDkYo3hbXGnXFLMBPJuEc3fKnzgTfwZose7cr1drceZsZ85npzbVhZ7HtGjUPZDX4vcLzm7k9Une8PbYchSLhKDUyptqYuRMaWJMSsGuKMPqcXeSw6rT58mtXeQnvTTuiJ9z9YbKxMW"
                          (hashGrammar \* (charIs '/' */ textIs "filename"))
                          (Right "SHA512/BAt7JLYgp8oAyt15RSTiA6QQGW3nHjU8/X7cUEDkYo3hbXGnXFLMBPJuEc3fKnzgTfwZose7cr1drceZsZ85npzbVhZ7HtGjUPZDX4vcLzm7k9Une8PbYchSLhKDUyptqYuRMaWJMSsGuKMPqcXeSw6rT58mtXeQnvTTuiJ9z9YbKxMW/filename")

    describe "Reading" $ do
      testReader $ ReadPathTestCase
                     "Single example"
                     "SHA512/BAt7JLYgp8oAyt15RSTiA6QQGW3nHjU8/X7cUEDkYo3hbXGnXFLMBPJuEc3fKnzgTfwZose7cr1drceZsZ85npzbVhZ7HtGjUPZDX4vcLzm7k9Une8PbYchSLhKDUyptqYuRMaWJMSsGuKMPqcXeSw6rT58mtXeQnvTTuiJ9z9YbKxMW/filename"
                     (hashGrammar \* (charIs '/' */ textIs "filename"))
                     ("", (\(Right h) -> Just h) . mkBase58 SHA512 $ "BAt7JLYgp8oAyt15RSTiA6QQGW3nHjU8X7cUEDkYo3hbXGnXFLMBPJuEc3fKnzgTfwZose7cr1drceZsZ85npzbVhZ7HtGjUPZDX4vcLzm7k9Une8PbYchSLhKDUyptqYuRMaWJMSsGuKMPqcXeSw6rT58mtXeQnvTTuiJ9z9YbKxMW")

hashGrammar :: PathPattern Hash
hashGrammar = hashIso \$/ (alg \* charIs '/')
                      \*/ hashTextBrokenAt32
  where
    hashIso :: Iso (HashAlgorithm, Text) Hash
    hashIso = Iso
      {_forwards  = \(alg,bytes) -> either (const Nothing) Just . mkBase58 alg $ bytes
      ,_backwards = Just . unBase58
      }

    alg :: Grammar HashAlgorithm
    alg = sha512

    sha512 :: Grammar HashAlgorithm
    sha512 = (textIs "SHA512" \|/ textIs "sha512") */ rpure SHA512

    hashTextBrokenAt32 :: Grammar Text
    hashTextBrokenAt32 = iso \$/ (takeNWhen 32 isHashCharacter)
                             \*/ (charIs '/' */ longestMatching isHashCharacter)
      where
        iso :: Iso (Text,Text) Text
        iso = Iso
          {_forwards  = \(prefix,suffix) -> Just $ prefix <> suffix
          ,_backwards = Just . Text.splitAt 32
          }

    takeNWhen :: Int -> (Char -> Bool) -> Grammar Text
    takeNWhen 0 _    = rpure ""
    takeNWhen n pred = consIso \$/ charWhen pred \*/ takeNWhen (n-1) pred

    consIso :: Iso (Char,Text) Text
    consIso = Iso
      { _forwards = \(c,t) -> Just . Text.cons c $ t
      , _backwards = \t -> Text.uncons t
      }

    hashCharacter :: Grammar Char
    hashCharacter = charWhen isHashCharacter

    isHashCharacter :: Char -> Bool
    isHashCharacter = (`elem` hashCharacters)

    -- Base58
    hashCharacters :: [Char]
    hashCharacters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

