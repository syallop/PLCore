{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , MultiWayIf
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : PL.ExprLike
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Classes of things which are like expressions. Types and kinds have a similar
structure.
-}
module PL.ExprLike where

import PL.Binds.Ix

import Data.Proxy

-- Class of 'e'xpression-like types which abstract over themselves.
class HasAbs e where
  -- Apply a function to abstracted sub 'e'xpression
  applyToAbs :: (e -> e) -> e -> e

class HasBinding e b where
  applyToBinding :: (b -> b) -> e -> e

-- Class of 'e'xpressions which have non-abstracted sub terms
class HasNonAbs e where
  applyToNonAbs :: (e -> e) -> e -> e


-- | A positive integer indicating how many abstractions a bound thing has been moved under.
newtype BuryDepth = BuryDepth {_unBurryDepth :: Int} deriving (Num,Eq)

-- Bury any escaping bindings in an 'e'xpression by a given depth.
--
-- E.G.
-- Unaffected as no bindings escape.
-- \.0        ~> \.0    --id
-- \.\.1      ~> \.\.1  --const
--
-- Escaping bindings are effected.
-- \.1        ~> \.(1+depth)
-- \.\.0 1 2  ~> \.\. 0 1 (2+depth)
buryBy :: forall e b. (HasAbs e,HasBinding e b,HasNonAbs e,BindingIx b) => Proxy b -> e -> BuryDepth -> e
buryBy _        e 0         = e
buryBy bindType e buryDepth = applyToAbs     (\subE     -> buryBetween 0 subE buryDepth)
                            . applyToBinding (\(b :: b) -> buryBinding b (_unBurryDepth buryDepth))
                            . applyToNonAbs  (\subE     -> buryBy bindType subE buryDepth)
                            $ e
  where
  buryBetween :: (HasAbs e,HasBinding e b,HasNonAbs e,BindingIx b) => Int -> e -> BuryDepth -> e
  buryBetween ourTop e buryDepth = applyToBinding (\(b :: b) -> if -- Binding is within our height.
                                                                   | bindDepth b <= ourTop -> b 
                                                                   -- Binding escapes our height, so compensate for the greater depth
                                                                   | otherwise             -> buryBinding b (_unBurryDepth buryDepth)
                                                  )
                                 . applyToAbs     (\subE -> buryBetween (ourTop+1) subE buryDepth)
                                 . applyToNonAbs  (\subE -> buryBetween ourTop subE buryDepth)
                                 $ e

