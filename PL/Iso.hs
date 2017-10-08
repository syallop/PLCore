{-|
Module      : PL.Iso
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

Partial Isomorphisms. Objects which translate back and forth between two types
with the possibility for failure. Round trips should not fail.
The two functions are named parse and print after their most frequent use.
-}
module PL.Iso
  ( Iso(..)
  , parseIso
  , printIso
  , inverseIso

  , identityI
  , composeIso

  , nilI
  , consI

  , unitI
  , flipI
  )
  where

import Control.Monad
import Control.Category

-- | An Iso converts both ways between a and b, each with an opportunity for
-- failure.
data Iso a b = Iso
  {_parseIso :: a -> Maybe b
  ,_printIso :: b -> Maybe a
  }

instance Category Iso where
  g . f = Iso (parseIso f >=> parseIso g)
              (printIso g >=> printIso f)
  id = Iso Just Just

parseIso :: Iso a b -> a -> Maybe b
parseIso (Iso parseI _) = parseI

printIso :: Iso a b -> b -> Maybe a
printIso (Iso _ printI) = printI

inverseIso :: Iso a b -> Iso b a
inverseIso (Iso parseI printI) = Iso printI parseI

-- A boring Iso which translates something to itself and back
identityI :: Iso a a
identityI = Iso Just Just

-- Isos can be composed
composeIso :: Iso a b -> Iso b c -> Iso a c
composeIso (Iso parseA printB) (Iso parseB printC)
  = Iso (parseA >=> parseB)
        (printC >=> printB)

nilI :: Iso () [a]
nilI = Iso
  (\() -> Just [])
  (\xs -> case xs of
    []     -> Just ()
    (x:xs) -> Nothing
  )

consI :: Iso (a,[a]) [a]
consI = Iso
  (\(x,xs) -> Just (x:xs))
  (\xs -> case xs of
    []     -> Nothing
    (x:xs) -> Just (x,xs)
  )

-- Unit element for products
unitI :: Iso a (a,())
unitI = Iso
  (\a -> Just (a,()))
  (\(a,()) -> Just a)

-- Products commute
flipI :: Iso (a,b) (b,a)
flipI = Iso
  (\(a,b) -> Just (b,a))
  (\(b,a) -> Just (a,b))

