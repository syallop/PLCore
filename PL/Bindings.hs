{-# LANGUAGE
    AllowAmbiguousTypes
  , MultiWayIf
  , OverloadedStrings
  , ScopedTypeVariables
  #-}
{-|
Module      : PL.Bindings
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.Bindings
  ( Bindings()
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
import Data.Monoid

import PL.Binds
import PL.Binds.Ix
import PL.ExprLike
import PL.Printer hiding (parens,between)
import PL.Printer.Doc


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

instance Document e
      => Document (Binding e) where
  document b = case b of
    Unbound -> DocText "U"
    Bound b -> DocText "B:" <> document b

instance Document e
      => Document (Bindings e) where
  document bs = between (char '[') (char ']') $ case bs of
                  EmptyBindings
                    -> emptyDoc

                  ConsBinding b bs
                    -> document b <> char ',' <> document bs

                  Buried bs
                    -> between (char '[') (char ']') $ document bs

-- | No bindings
emptyBindings :: Bindings e
emptyBindings = EmptyBindings

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
bindAll = foldr ((.) . bind) id

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
index bindType bs ix = fromMaybe (error "index: given ix is not contained in the bindings") $ safeIndex bindType bs ix

