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
import PLPrinter

import Data.Monoid

-- | Debrujn index for referencing variables.
-- A positive integer describing how many abstractions deep the var is found.
data Var = VZ | VS Var
  deriving (Eq, Ord)

instance Show Var where
  show = show . bindDepth

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

instance Binds Var typ where

  data BindCtx Var typ = VarCtx [typ]

  emptyCtx :: BindCtx Var typ
  emptyCtx = VarCtx []

  addBinding :: typ -> BindCtx Var typ -> BindCtx Var typ
  addBinding t (VarCtx ts) = VarCtx (t:ts)

  lookupBindingTy :: Var -> BindCtx Var typ -> Maybe typ
  lookupBindingTy b (VarCtx ts) =  Just $ ts !! bindDepth b

  toList :: BindCtx Var typ -> [(Var,typ)]
  toList (VarCtx ts) = enumFrom VZ `zip` ts

instance BindingIx Var where
  bindDepth :: Var -> Int
  bindDepth VZ     = 0
  bindDepth (VS n) = 1+bindDepth n

  buryBinding :: Var -> Int -> Var
  buryBinding v 0 = v
  buryBinding v n = buryBinding (VS v) (n-1)

type family BindingFor     phase
