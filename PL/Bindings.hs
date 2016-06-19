{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PL.Bindings
  ( BuryDepth()
  , Bindings()
  , Binding(Unbound,Bound)
  , emptyBindings
  , bury
  , bind
  , unbound

  , safeIndex
  , index

  , buryN
  , bindAll
  , bindFromList

  , buryBy
  )
  where

import Control.Applicative
import Data.Maybe
import Data.Proxy

import PL.Binds
import PL.ExprLike

-- | A positive integer indicating how many abstractions a bound thing has been moved under.
newtype BuryDepth = BuryDepth {_unBurryDepth :: Int} deriving (Num,Eq)

data Binding e
  = Unbound -- No expression bound to this abstraction (/yet)
  | Bound e -- This expression is bound to the abstraction
  deriving Show

-- A context of bound things 'e' in which you may:
-- - 'bind'    : E.G. when an expr is applied to a lambda abstraction
-- - 'unbound' : E.G. when you want to manipulate expressions under a lambda abstraction whose binding hasnt been applied (/yet).
-- - 'bury'    : E.G. when all of the bindings are lifted under an abstraction, any escaping bindings should be burried.
data Bindings e
  = EmptyBindings                        -- ^ No bindings
  | ConsBinding (Binding e) (Bindings e) -- ^ A new binding
  | Buried (Bindings e)                  -- ^ Bury many bindings beneath a lambda abstraction
  deriving Show

-- | No bindings
emptyBindings :: Bindings e
emptyBindings = emptyBindings

-- | Bury bindings under an abstraction
bury :: Bindings e -> Bindings e
bury = Buried

-- | Bury bindings under a number of abstractions
buryN :: Int -> Bindings e -> Bindings e
buryN 0 = id
buryN n = buryN (n-1) . Buried

-- | An unbound 'e'
unbound :: Bindings e -> Bindings e
unbound = ConsBinding Unbound

-- | Bind 'e'
bind :: e -> Bindings e -> Bindings e
bind = ConsBinding . Bound

-- | Binds 'e's
bindAll :: [e] -> Bindings e -> Bindings e
bindAll []     = id
bindAll (e:es) = bind e . bindAll es

-- | Create a Bindings from a list of bound 'e's
bindFromList :: [e] -> Bindings e
bindFromList = (`bindAll` emptyBindings)

-- | If the index exists then extract the bound 'e', adjusting it for its depth in the bindings
safeIndex :: forall e b. (HasAbs e,HasBinding e b,HasNonAbs e,BindingIx b) => Proxy b -> Bindings e -> Int -> Maybe (Binding e)
safeIndex bindType EmptyBindings _  = Nothing
safeIndex bindType bs ix
  | ix < 0    = Nothing
  | otherwise = safeIndex' 0 bs ix
  where
  safeIndex' :: (HasAbs e,HasBinding e b,HasNonAbs e,BindingIx b) => BuryDepth -> Bindings e -> Int -> Maybe (Binding e)
  safeIndex' buryDepth bs ix = case (bs,ix) of
    (EmptyBindings, _)
      -> Nothing

    (ConsBinding b _, 0)
      -> case b of
           Unbound -> Just Unbound
           Bound e -> Just $ Bound $ buryBy bindType e buryDepth

    (ConsBinding _ bs', _)
      -> safeIndex' buryDepth bs' (ix-1)

    (Buried bs', _)
      -> safeIndex' (buryDepth + 1) bs' ix

-- | A 'safeIndex' assuming the index is contained in the bindings.
index :: (HasAbs e,HasBinding e b,HasNonAbs e,BindingIx b) => Proxy b -> Bindings e -> Int -> Binding e
index bindType bs = fromJust . safeIndex bindType bs




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

