{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PL.TyVar where

import PL.Var
import PL.Binds
import PL.Binds.Ix
import PL.Kind

import Data.Coerce

newtype TyVar = TyVar {_unTyVar :: Var}
  deriving (Eq,Ord,Enum)

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

  emptyCtx :: BindCtx TyVar Kind
  emptyCtx = TyVarCtx []

  addBinding :: Kind -> BindCtx TyVar Kind -> BindCtx TyVar Kind
  addBinding k (TyVarCtx ks) = TyVarCtx (k:ks)

  lookupBindingTy :: TyVar -> BindCtx TyVar Kind -> Maybe Kind
  lookupBindingTy b (TyVarCtx ks) = Just $ ks !! (bindDepth b)

  toList :: BindCtx TyVar Kind -> [(TyVar,Kind)]
  toList (TyVarCtx ks) = (enumFrom (TyVar VZ)) `zip` ks

instance BindingIx TyVar where
  bindDepth :: TyVar -> Int
  bindDepth tv = bindDepth (coerce tv :: Var)

  buryBinding :: TyVar -> Int -> TyVar
  buryBinding tv n = coerce $ buryBinding (coerce tv :: Var) n

