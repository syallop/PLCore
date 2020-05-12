{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RankNTypes
           , OverloadedStrings
  #-}
{-|
Module      : PL.Store.File
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Implementation of PL.Store backed by a file system.
-}
module PL.Store.File
  ( FileStore ()
  , newFileStore
  , ensureInitialized
  , storeAsFile
  , lookupFromFile
  )
  where

import Prelude hiding (lookup, readFile, writeFile)

import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString
import System.FilePath.ByteString
import System.Directory

import PL.Store
import PL.Serialize

-- | Store key-values under a particular location in the filesystem.
data FileStore k v = FileStore
  { _directory :: ByteString
  }
  deriving Show

-- | Create a filestore from a directory path
newFileStore :: ByteString -> FileStore k v
newFileStore dir = FileStore dir

instance (Serialize k, Serialize v, Eq v, Ord v) => Store FileStore k v where
  store  = storeAsFile
  lookup = lookupFromFile

-- | Store a key-value association by serializing the value and writing it to a
-- file in the filestore named by the key.
storeAsFile
  :: ( Serialize k
     , Serialize v
     , Eq v
     , Ord v
     )
  => FileStore k v
  -> k
  -> v
  -> IO (Maybe (FileStore k v, StoreResult v))
storeAsFile filestore key value = do
  -- TODO: Consider mapping relevant filesystem exceptions to Nothing.
  let keyPath         = keyFilePath filestore key
      serializedValue = serialize value

  alreadyExists <- doesPathExist keyPath
  if alreadyExists
    -- A value is already stored at this key.
    -- If it contains the same content, we don't need to do anything.
    -- If it differs we replace and return the old value.
    then do existingBytes <- readFile keyPath
            case deserialize existingBytes of
              -- Whatever is in the file does not deserialize.
              -- Either:
              -- - Serialization does not round trip correctly
              -- - The file has been tampered with
              -- - We have a hash collision
              -- Fail loudly for now.
              Nothing
                -> error $ mconcat [ "When attempting to store a key-value in the filesystem store we encountered an existing file which did not deserialize as expected. This could indicate:\n"
                                   , "- Serialization does not round trip correctly\n"
                                   , "- The file has been tampered file\n"
                                   , "- We have a hash collision\n"
                                   , "\n"
                                   , "The file in question is at path: "
                                   , keyPath
                                   ]

              Just existingValue
                | existingValue == value
                 -> pure $ Just (filestore, AlreadyStored)

                | otherwise
                 -> do writeFile keyPath serializedValue
                       pure $ Just (filestore, Overwritten $ Set.fromList [existingValue])

    -- Key is not stored already.
    else do writeFile keyPath serializedValue
            pure $ Just (filestore, Successfully)


-- | Read a value by consulting a file in the filestore named by the key.
lookupFromFile
  :: ( Serialize k
     , Serialize v
     )
  => FileStore k v
  -> k
  -> IO (Maybe (FileStore k v, v))
lookupFromFile filestore key = do
  let keyPath = keyFilePath filestore key

  exists <- doesFileExist keyPath
  if not exists
    then pure Nothing
    else do fileBytes <- readFile (keyFilePath filestore key)
            case deserialize fileBytes of
              Nothing
                -> pure Nothing
              Just value
                -> pure $ Just $ (filestore, value)

-- Compute a keys file path in a file store.
keyFilePath :: Serialize k => FileStore k v -> k -> FilePath
keyFilePath (FileStore dir) key = decodeFilePath $ dir <> "/" <> serialize key

-- Ensure a FileStores directory exists - create it if it does not.
ensureInitialized :: FileStore k v -> IO ()
ensureInitialized (FileStore dir) = createDirectoryIfMissing True $ decodeFilePath dir

