{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : PL.Store.File.Path
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Define patterns for filepaths to primarily support two operations:
- generatePath - which transforms a key into a path
- readPathKey  - which extracts a key from a path

The internal implementation is _mostly_ hidden as it's unneccesarily complex and isnt quite 'right'.

To create simple patterns, use mkPathPattern.

For more complex patterns, use the helpers 'pathSegment', pathSegments' and fileName and combine with '*/' '\*'.
For more power, peek behind the type alias and write Grammars directly.

-}
module PL.Store.File.Path
  ( -- simpler API
    PathPattern
  , mkPathPattern
  , generatePath
  , readPathKey

  -- Build Path Patterns
  , pathSegments
  , pathSegment
  , fileName
  , segmented
  )
  where

import Control.Monad
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

import PL.Error

import PLPrinter.Doc
import PLGrammar
import Reversible
import Reversible.Iso

-- TODO: Consider how much of the prefix/ length/ breaking logic can be moved/ belongs in
-- the ISO.

-- | A PathPattern describes how a Textual key is formatted into a Path.
type PathPattern k = Grammar k

-- | Create a path pattern in a common format, building paths of the form:
--
-- prefix/sub/directories/KEYASTEXT/fileName
--
-- When the fixed key length is greater than 32, the key name will be broken
-- into subdirectories with length no greater than 32.
mkPathPattern
  :: Show k
  => Iso Text k    -- ^ Transform some key into a textual representation
  -> Int           -- ^ Fixed key length.
  -> Text          -- ^ File name
  -> PathPattern k
mkPathPattern keyToText keyLength file = keyToText \$/ segmented keyLength breakAt \* fileName file
  where
    breakAt
      | keyLength <= 32 = 32
      | otherwise       = 32

{- Utils for building path patterns -}

pathSegment :: Text -> PathPattern ()
pathSegment txt = textIs txt \* charIs '/'

pathSegments :: [Text] -> PathPattern ()
pathSegments []     = rpure ()
pathSegments (p:ps) = pathSegment p */ pathSegments ps

fileName :: Text -> PathPattern ()
fileName = textIs

-- Read Text exactly n characters long with any amount of intermediate /s that
-- don't count towards the length.
segmented
  :: Int
  -> Int
  -> PathPattern Text
segmented total break = segmented' total break
  where
  segmented' :: Int -> Int -> Grammar Text
  segmented' 0           _break = textIs "/" */ rpure ""
  segmented' totalLength 0      = charIs '/' */ segmented' totalLength break
  segmented' totalLength break  = consIso \$/ (charWhen (/= '/')) \*/ (segmented' (totalLength-1) (break-1))

  consIso :: Iso (Char,Text) Text
  consIso = Iso
    { _forwards = \(c,t) -> Just . Text.cons c $ t
    , _backwards = \t -> Text.uncons t
    }

-- | Given a key and a pathpattern, generate the corresponding path.
generatePath
  :: Show k
  => k
  -> PathPattern k
  -> Either (ErrorFor phase) FilePath
generatePath k pattern =
  let PathGenerator f = toPathGenerator pattern
   in Text.unpack <$> f k

-- A PathGenerator transforms some key into a possible path where it can be
-- stored.
newtype PathGenerator k = PathGenerator (forall phase. k -> Either (ErrorFor phase) Text)

-- Convert a PathPattern into a Generator by interpreting it 'backwards'.
toPathGenerator
  :: Show k
  => PathPattern k
  -> PathGenerator k
toPathGenerator (Reversible g) = case g of
  ReversibleInstr i
    -> case i of
         GAnyChar
           -> PathGenerator $ \k -> Right $ Text.singleton k

         -- TODO:
         GLabel _ g
           -> toPathGenerator g
         GTry g
           -> toPathGenerator g

  RPure a
    -> PathGenerator $ \a' -> if a == a'
                                then Right mempty
                                else Left . EMsg . mconcat $
                                       [ text "When generating path we expected to see:"
                                       , lineBreak
                                       , indent1 . string . show $ a
                                       , lineBreak
                                       , text "But we saw:"
                                       , lineBreak
                                       , indent1 . string . show $ a'
                                       ]

  REmpty
    -> PathGenerator $ \a -> Left . EMsg . mconcat $
                               [ text "Aborted generating path. Next input:"
                               , lineBreak
                               , indent1 . string . show $ a
                               ]

  RAlt g0 g1
    -> PathGenerator $ \a -> let PathGenerator p = toPathGenerator g0
                                 PathGenerator q = toPathGenerator g1
                              in case p a of
                                   Left err
                                     -> q a
                                   Right x
                                     -> Right x

  RMap iso ga
    -> PathGenerator $ let PathGenerator p = toPathGenerator ga
                        in \a -> case backwards iso a of
                                   Nothing
                                     -> Left . EMsg . mconcat $
                                          [ text $ "Failed to apply function in reverse to:"
                                          , indent1 . string . show $ a
                                          ]
                                   Just x
                                     -> p x

  RAp ga gb
    -> PathGenerator $ let PathGenerator p = toPathGenerator ga
                           PathGenerator q = toPathGenerator gb
                        in \(a,b) -> liftM2 mappend (p a) (q b)

-- | Attempt to read a path against a pattern, producing leftovers and whether
-- the parse succeeded or failed.
readPathKey
  :: Text
  -> PathPattern k
  -> (Text, Maybe k)
readPathKey txt pattern = runPathReader txt . toPathReader $ pattern

-- Read a path, extracting a possible key.
newtype PathReader k = PathReader (Text -> (Text, Maybe k))

runPathReader
  :: Text
  -> PathReader k
  -> (Text, Maybe k)
runPathReader txt (PathReader p) = p txt

instance Semigroup a => Semigroup (PathReader a) where
  pa0 <> pa1 = do
    a0 <- pa0
    a1 <- pa1
    pure (a0 <> a1)

instance Monoid a => Monoid (PathReader a) where
  mempty = return mempty

instance Functor PathReader where
  fmap f (PathReader pa) = PathReader $ \txt -> case pa txt of
    (leftovers,Just a)
      -> (leftovers,Just $ f a)

    (leftovers,Nothing)
      -> (leftovers,Nothing)

instance Applicative PathReader where
  pure  = return
  (<*>) = ap

instance Monad PathReader where
  return a = PathReader $ \txt -> (txt, Just a)

  (PathReader pa) >>= f = PathReader $ \txt -> case pa txt of
    (leftovers, Nothing)
      -> (leftovers, Nothing)

    (leftovers, Just a)
      -> let PathReader pb = f a
          in pb leftovers

instance Alternative PathReader where
  empty = mzero
  (<|>) = mplus

instance MonadPlus PathReader where
  mzero = PathReader $ \txt -> (txt,Nothing)

  mplus pa0 pa1 = PathReader $ \txt0 -> case runPathReader txt0 pa0 of
    -- If input has been consumed, don't try the second
    (txt1,Nothing)
      | txt0 == txt1 -> runPathReader txt0 pa1
      | otherwise    -> (txt1,Nothing)

    (txt1,Just a)
      -> (txt1, Just a)

-- Convert a pathpattern into a pathreader by interpreting it 'forwards'.
toPathReader :: PathPattern a -> PathReader a
toPathReader (Reversible g) = case g of
  ReversibleInstr i
    -> case i of
         GAnyChar
           -> PathReader $ \txt -> case Text.uncons txt of
                Nothing
                  -> (txt,Nothing)
                Just (c,txt')
                  -> (txt',Just c)

         GLabel _ g
           -> PathReader $ \txt -> case runPathReader txt (toPathReader g) of
                (leftovers,Nothing)
                  -> (leftovers,Nothing)
                (leftovers,Just a)
                  -> (leftovers,Just a)

         GTry g
           -> PathReader $ \txt ->  case runPathReader txt (toPathReader g) of
                (_leftovers,Nothing)
                  -> (txt,Nothing)
                (leftovers,Just a)
                  -> (leftovers,Just a)
  RPure a
    -> PathReader $ \txt -> (txt, Just a)

  REmpty
    -> PathReader $ \txt -> (txt, Nothing)

  RAlt g0 g1
    -> toPathReader g0 <|> toPathReader g1

  RMap iso ga
    -> let PathReader p = toPathReader ga
        in PathReader $ \txt -> case p txt of
             (leftovers,Nothing)
               -> (leftovers,Nothing)

             (leftovers,Just res)
               -> case forwards iso res of
                    Nothing
                      -> (leftovers,Nothing)
                    Just b
                      -> (leftovers,Just b)

  RAp ga gb
    -> (,) <$> toPathReader ga <*> toPathReader gb

