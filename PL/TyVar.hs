{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , InstanceSigs
  , MultiParamTypeClasses
  , TypeFamilies
  #-}
{-|
Module      : PL.TyVar
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Type-level variables which can be used with a binding context to Kinds.
-}
module PL.TyVar where

-- PL
import PL.Binds
import PL.Binds.Ix
import PL.Kind
import PL.Var

-- External PL
import PLHash
import PLPrinter

-- Other
import Data.Coerce

newtype TyVar = TyVar {_unTyVar :: Var}
  deriving (Eq,Ord,Enum,Document)

instance Show TyVar where
  show (TyVar v) = show v

mkTyVar :: Int -> TyVar
mkTyVar = TyVar . mkVar

tvzero,tvone,tvtwo,tvthree,tvfour :: TyVar
tvzero  = coerce vzero
tvone   = coerce vone
tvtwo   = coerce vtwo
tvthree = coerce vthree
tvfour  = coerce vfour

instance Binds TyVar Kind where

  data BindCtx TyVar Kind = TyVarCtx [Kind]
    deriving (Eq, Ord)

  emptyCtx :: BindCtx TyVar Kind
  emptyCtx = TyVarCtx []

  addBinding :: Kind -> BindCtx TyVar Kind -> BindCtx TyVar Kind
  addBinding k (TyVarCtx ks) = TyVarCtx (k:ks)

  lookupBindingTy :: TyVar -> BindCtx TyVar Kind -> Maybe Kind
  lookupBindingTy b (TyVarCtx ks) = case drop (bindDepth b) ks of
    k:_ -> Just k
    []  -> Nothing

  toList :: BindCtx TyVar Kind -> [(TyVar,Kind)]
  toList (TyVarCtx ks) = enumFrom (TyVar VZ) `zip` ks

instance BindingIx TyVar where
  bindDepth :: TyVar -> Int
  bindDepth tv = bindDepth (coerce tv :: Var)

  buryBinding :: TyVar -> Int -> TyVar
  buryBinding tv n = coerce $ buryBinding (coerce tv :: Var) n

instance Hashable TyVar where
  toHashToken = HashInt . bindDepth
