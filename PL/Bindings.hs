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

Data structure designed to hold values for bindings which may be:
- Bound with some value
- Unbound with no value yet
- Burried as if the expression they correspond to has been moved under an
  abstraction, such as under a lambda.

This abstraction is not (currently) used to lookup metadata such as bindings
types or type bindings kinds, this is handled by Binds which is confusingly
similar. It's possible these abstractions should be unified.

In short:
Binds:
- Maps binding constructs (such as variables, type variables) to metadata such
  as the type for variables and the kind for type variables.
- Is used in the typechecking/ kind checking phase before reduction as the
   interface does not allow for bindings to be moved under each other.
Whereas Bindings:
- Map Binding indexes (which variables, type variables may be/ contain) to
  either their bound value, or an Unbound token.
- Is used in the reduction/ evaluation phase as the interface allows for
  bindings to be moved under each other as functions are applied.

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

  , ppBinding
  , ppBindings
  , ppBindingsTree
  )
  where

import Control.Applicative
import Data.Maybe
import Data.Proxy
import Data.Text (Text,unpack,pack)
import Data.Monoid

import Data.Tree
import Data.Tree.Pretty

import PL.Binds
import PL.Binds.Ix
import PL.ExprLike
import PLPrinter hiding (parens,between)
import PLPrinter.Doc


data Binding e
  = Unbound -- No expression bound to this abstraction (/yet)
  | Bound e -- This expression is bound to the abstraction
  deriving (Show,Eq,Ord)

-- A context of bound things 'e' in which you may:
-- - 'bind'    : E.G. when an expr is applied to a lambda abstraction
-- - 'unbound' : E.G. when you want to manipulate expressions under a lambda abstraction whose binding hasnt been applied (/yet).
-- - 'bury'    : E.G. when all of the bindings are lifted under an abstraction, any escaping bindings should be burried.
data Bindings e
  = EmptyBindings                        -- ^ No bindings
  | ConsBinding (Binding e) (Bindings e) -- ^ A new binding
  | Buried (Bindings e)                  -- ^ Bury many bindings beneath a lambda abstraction
  deriving (Show,Eq,Ord)

instance Document e
      => Document (Binding e) where
  document = ppBinding document

instance Document e
      => Document (Bindings e) where
  document = ppBindings document

ppBinding
  :: (e -> Doc)
  -> Binding e
  -> Doc
ppBinding ppExpr b = case b of
  Unbound
    -> text "?"
  Bound e
    -> ppExpr e

ppBindings
  :: (e -> Doc)
  -> Bindings e
  -> Doc
ppBindings ppExpr bs  = between (char '[') (char ']') $ case bs of
  EmptyBindings
    -> emptyDoc

  ConsBinding b bs
    -> ppBinding ppExpr b <> char ',' <> ppBindings ppExpr bs

  Buried bs
    -> between (char '[') (char ']') $ ppBindings ppExpr bs

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
    -- We don't have any more bindings and so we've failed to lookup.
    (EmptyBindings, _)
      -> Nothing

    -- We've found the binding.
    (ConsBinding b _, 0)
      -> case b of
           -- But it hasn't been bound.
           Unbound
             -> Just Unbound

           -- It's been bound. Adjust.
           Bound e
             -> Just $ Bound $ buryBy bindType e buryDepth

    -- At least one more away
    (ConsBinding _ bs', _)
      -> safeIndex' buryDepth bs' (ix-1)

    (Buried bs', _)
      -> safeIndex' (buryDepth + 1) bs' ix

-- | A 'safeIndex' assuming the index is contained in the bindings.
index :: (HasAbs e,HasBinding e b,HasNonAbs e,BindingIx b) => Proxy b -> Bindings e -> Int -> Binding e
index bindType bs ix = fromMaybe (error "index: given ix is not contained in the bindings") $ safeIndex bindType bs ix

ppBindingsTree
  :: (e -> Doc)
  -> Bindings e
  -> Doc
ppBindingsTree ppExpr bs = rawText $ pack $ drawVerticalTree $ ppBindingsTree' ppExpr bs
 where
   ppBindingsTree'
     :: (e -> Doc)
     -> Bindings e
     -> Tree String
   ppBindingsTree' ppExpr bs = case bs of
     EmptyBindings
       -> Node "Empty" []

     ConsBinding b bs
       -> Node "Cons" [ (Node "Binding" [Node (unpack . render . ppBinding ppExpr $ b) []])
                      , ppBindingsTree' ppExpr bs
                      ]

     Buried bs
       -> Node "Buried" [ppBindingsTree' ppExpr bs]

