{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RankNTypes
           , OverloadedStrings
           , GADTs
           , TupleSections
           , ScopedTypeVariables
  #-}
{-|
Module      : PL.Store.File
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Implementation of PL.Store backed by a file system.
-}
module PL.Store.File
  ( FileStore (..)
  , newFileStore
  , newSimpleFileStore
  , mkPathPattern

  , storeAsFile
  , lookupFromFile

  -- Util
  , ensureInitialized
  )
  where

import Prelude hiding (lookup, readFile, writeFile, take, drop)

import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString, readFile, writeFile)
import qualified Data.List as List
import System.FilePath.ByteString
import System.Directory
import Data.Traversable
import System.Directory.Tree (
    AnchoredDirTree(..), DirTree(..),
    filterDir, readDirectoryWith
    )
import System.FilePath (takeExtension)
import Data.Foldable
import Control.Monad

import PL.Store
import PL.ShortStore
import PL.Serialize
import PL.Error
import PL.Store.File.Path

import PLPrinter.Doc

import Reversible.Iso

-- | Store key-values under a particular location in the filesystem.
--
-- Re-initialising a store with a different prefix should have no negative
-- effect (other than making previous files inaccessbile). There is currently no
-- attempt to detect or mitigate this.
data FileStore k v = Ord v => FileStore
  { _subDirectories       :: [ByteString]
  , _filePattern          :: PathPattern k
  , _serializeFileBytes   :: v -> ByteString
  , _deserializeFileBytes :: forall phase. ByteString -> Either (ErrorFor phase) v
  , _valuesEqual          :: v -> v -> Bool
  }

instance Show (FileStore k v) where
  show _ = "FileStore"

-- | Create a FileStore where values are stored:
-- - Under optional subdirectories
-- - Under a fixed length key, broken with subdirectories if 'too long'
-- - In a file with a constant name.
--
-- E.G.
-- Prefix: some,prefix
-- Key: KEYNAMEBROKEN
-- KeyLength: 12
-- Internal definition of 'too long': 3
-- File: file
--
-- Result path: some/prefix/KEY/NAM/EBR/OKE/N/file
newSimpleFileStore
  :: forall k v
   . ( Serialize k
     , Serialize v
     , Show k
     , Eq v
     , Ord v
     )
  => [ByteString] -- ^ Optional subdirectory to store under
  -> Int           -- ^ Fix key length
  -> Text
  -> FileStore k v
newSimpleFileStore subDirs keyLength fileName = newFileStore subDirs (mkPathPattern iso keyLength fileName) serialize deserialize (==)
  where
    iso :: Iso Text k
    iso = Iso
      {_forwards  = \txt-> either (const Nothing) Just . deserialize . encodeUtf8 $ txt
      ,_backwards = Just . decodeUtf8 . serialize
      }

newFileStore
  :: Ord v
  => [ByteString]
  -> PathPattern k
  -> (v -> ByteString)
  -> (forall phase. ByteString -> Either (ErrorFor phase) v)
  -> (v -> v -> Bool)
  -> FileStore k v
newFileStore subDirs filePattern serializeBytes deserializeBytes valuesEqual = FileStore
  { _subDirectories       = subDirs
  , _filePattern          = filePattern
  , _serializeFileBytes   = serializeBytes
  , _deserializeFileBytes = deserializeBytes
  , _valuesEqual          = valuesEqual
  }

instance
  (Ord v, Show k)
   => Store FileStore k v where
  store  = storeAsFile
  lookup = lookupFromFile

baseDirectory
  :: FileStore k v
  -> FilePath
baseDirectory = decodeFilePath . (<> "/") . mconcat . List.intersperse "/" . _subDirectories

-- | Generate the file path associated with a key
generateFullPath
  :: Show k
  => k
  -> FileStore k v
  -> Either (ErrorFor phase) FilePath
generateFullPath key f = case generatePath key (_filePattern f) of
  Left err
    -> Left err

  Right path
    -> Right $ baseDirectory f <> path

-- | Store a key-value association by serializing the value and writing it to a
-- file in the filestore named by the key.
storeAsFile
  :: (Ord v
     ,Show k
     )
  => FileStore k v
  -> k
  -> v
  -> IO (Either (ErrorFor phase) (FileStore k v, StoreResult v))
storeAsFile filestore key value = do
  -- TODO: Consider mapping relevant filesystem exceptions to Nothing.
  case generateFullPath key filestore of
    Left err
      -> pure . Left . EContext (EMsg . mconcat $ [ text "File store cannot generate path for key:"
                                                  , lineBreak
                                                  , indent1 . string . show $ key
                                                  ]) $ err

    Right keyPath
      -> do let serializedValue = _serializeFileBytes filestore value
            alreadyExists <- doesPathExist keyPath
            if alreadyExists
              -- A value is already stored at this key.
              -- If it contains the same content, we don't need to do anything.
              -- If it differs we replace and return the old value.
              then do existingBytes <- readFile keyPath
                      case _deserializeFileBytes filestore existingBytes of
                        -- Whatever is in the file does not deserialize.
                        -- Either:
                        -- - Serialization does not round trip correctly
                        -- - The file has been tampered with
                        -- - We have a hash collision
                        Left err
                          -> pure . Left . EContext (EMsg . mconcat $
                               [ text "When attempting to store a key-value in the filesystem store we encountered an existing file which did not deserialize as expected. This could indicate:"
                               , lineBreak
                               , text "- Serialization does not round trip correctly"
                               , lineBreak
                               , text "- The file has been tampered file"
                               , lineBreak
                               , text "- We have a hash collision"
                               , lineBreak,lineBreak
                               , text "The file in question is at path: "
                               , lineBreak
                               , string . show $ keyPath
                               ])
                               $ err

                        Right existingValue
                          | _valuesEqual filestore existingValue value
                           -> pure . Right $ (filestore, AlreadyStored)

                          | otherwise
                           -> do writeFileAndAnyMissingDirs keyPath serializedValue
                                 pure . Right $ (filestore, Overwritten $ Set.fromList [existingValue])

              -- Key is not stored already.
              else do writeFileAndAnyMissingDirs keyPath serializedValue
                      pure . Right $ (filestore, Successfully)

-- | Read a value by consulting a file in the filestore named by the key.
lookupFromFile
  :: Show k
  => FileStore k v
  -> k
  -> IO (Either (ErrorFor phase) (FileStore k v, Maybe v))
lookupFromFile filestore key = do
  case generateFullPath key filestore of
    Left err
      -> pure . Left . EContext (EMsg . mconcat $ [ text "File store cannot generate path for key:"
                                                  , lineBreak
                                                  ,  indent1 . string . show $ key
                                                  ]) $ err

    Right keyPath
      -> do exists <- doesFileExist keyPath
            if not exists
              then pure . Right $ (filestore, Nothing)
              else do fileBytes <- readFile keyPath
                      case _deserializeFileBytes filestore fileBytes of
                        Left err
                          -> pure . Left . EContext (EMsg . mconcat $
                               [ text "File store cannot deserialize the file associated with a path to the expected value type:"
                               , lineBreak
                               , indent1 . mconcat $
                                   [ text "Key:"
                                   , lineBreak
                                   , indent1 . string . show $ key
                                   , lineBreak

                                   , text "Path:"
                                   , lineBreak
                                   , indent1 . string . show $ keyPath
                                   , lineBreak

                                   , text "File bytes:"
                                   , lineBreak
                                   , indent1 . string . show $ fileBytes
                                   ]
                               ])
                               $ err

                        Right value
                          -> pure .  Right $ (filestore, Just value)

-- Ensure a FileStores directory exists - create it if it does not.
ensureInitialized :: FilePath -> IO ()
ensureInitialized filepath = createDirectoryIfMissing True filepath

-- Write a file including any missing subdirectorys in the path.
writeFileAndAnyMissingDirs :: FilePath -> ByteString -> IO ()
writeFileAndAnyMissingDirs filepath value = do
  let dir = takeDirectory $ encodeFilePath filepath
  createDirectoryIfMissing True $ decodeFilePath dir
  writeFile filepath value

instance
  ( Ord v
  , Ord shortK
  , Shortable k shortK


  , Show shortK
  , Show k
  ) => ShortStore FileStore k shortK v where
  largerKeys s shortK = Right . (s,) <$> largerKeysFromFiles s shortK
  shortenKey s k      = Right . (s,) <$> shortenKeyFromFiles s k


-- | Lookup all known keys that are 'larger' than a short key.
largerKeysFromFiles
  :: ( Show k
     , Ord shortK
     , Shortable k shortK
     )
  => FileStore k v
  -> shortK
  -> IO [k]
largerKeysFromFiles f shortKey = filter (\key -> shortKey <= toShort key) <$> getAllKeys f
-- TODO: We should be able to search along promising paths rather than grabbing
-- every path and checking it.

-- | Shorten a key to the smallest possible unambiguous ShortKey.
shortenKeyFromFiles
  :: (Show shortK, Shortable k shortK)
  => FileStore k v
  -> k
  -> IO shortK
shortenKeyFromFiles f key = do
  allKeys <- getAllKeys f
  case fmap (shortenAgainst key . Just) allKeys of
    []
      -> pure . shortenAgainst key $ Nothing

    xs
      -> do let shortenings = List.sortOn shortLength $ xs
            pure . head $ shortenings
-- TODO: We should be able to compare against less keys.

-- Get all keys stored in the FileStore.
getAllKeys
  :: FileStore k v
  -> IO [k]
getAllKeys f = do
  -- Compute the directory tree for the subdirectory, tagging each file with
  -- it's path _excluding_ the subdirectory.
  let base = baseDirectory f
  _:/tree <- readDirectoryWith (\filePath -> pure . Text.pack . List.drop (Prelude.length base) $ filePath) base

  -- Filter out invalid files and directories
  let allFiles = filterDir isValidFile tree

  -- Filter out files which don't match our filestores pattern.
  allKeys <- traverse (\filePath -> pure . readPathKey filePath $ (_filePattern f)) $ allFiles
  pure $ foldr (\(leftovers,mKey) acc
                 -> if leftovers /= ""
                      then acc
                      else case mKey of
                             Nothing
                               -> acc

                             Just key
                               -> key : acc
                )
                [] allKeys
  where
    isValidFile :: DirTree a -> Bool
    isValidFile t = case t of
      Dir ('.':_) _
        -> False

      File n _
        -> True
      _ -> True

