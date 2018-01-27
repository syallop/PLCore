{-# LANGUAGE
     FlexibleInstances
   , InstanceSigs
   , MultiParamTypeClasses
   , TypeFamilies
   #-}
{-|
Module      : PL.Var
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Variables which can be used within a binding context to Types.
-}
module PL.Var where

import PL.Binds
import PL.Binds.Ix
import PL.PLPrinter.Printer
import PL.Type

import Data.Monoid

-- | Debrujn index for referencing variables.
-- A positive integer describing how many abstractions deep the var is found.
data Var = VZ | VS Var
  deriving (Eq, Ord, Show)

instance Document Var where
  document = int . bindDepth

instance Enum Var where
  toEnum 0 = VZ
  toEnum i = VS (toEnum (i-1))

  fromEnum VZ     = 0
  fromEnum (VS v) = 1 + fromEnum v

mkVar :: Int -> Var
mkVar 0 = VZ
mkVar n = VS (mkVar (n-1))

vzero  = VZ
vone   = VS vzero
vtwo   = VS vone
vthree = VS vtwo
vfour  = VS vthree

instance Binds Var (Type tb) where

  data BindCtx Var (Type tb) = VarCtx [Type tb]

  emptyCtx :: BindCtx Var (Type tb)
  emptyCtx = VarCtx []

  addBinding :: Type tb -> BindCtx Var (Type tb) -> BindCtx Var (Type tb)
  addBinding t (VarCtx ts) = VarCtx (t:ts)

  lookupBindingTy :: Var -> BindCtx Var (Type tb) -> Maybe (Type tb)
  lookupBindingTy b (VarCtx ts) =  Just $ ts !! bindDepth b

  toList :: BindCtx Var (Type tb) -> [(Var,Type tb)]
  toList (VarCtx ts) = enumFrom VZ `zip` ts

instance BindingIx Var where
  bindDepth :: Var -> Int
  bindDepth VZ     = 0
  bindDepth (VS n) = 1+bindDepth n

  buryBinding :: Var -> Int -> Var
  buryBinding v 0 = v
  buryBinding v n = buryBinding (VS v) (n-1)

