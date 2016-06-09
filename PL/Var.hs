{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module PL.Var where

import PL.Binds
import PL.Type

-- | Debrujn index for referencing variables.
-- A positive integer describing how many abstractions deep the var is found.
data Var = VZ | VS Var
  deriving (Show, Eq, Ord)

mkVar :: Int -> Var
mkVar 0 = VZ
mkVar n = VS (mkVar (n-1))

vzero  = VZ
vone   = VS vzero
vtwo   = VS vone
vthree = VS vtwo
vfour  = VS vthree

instance (Show tb,Eq tb) => Binds Var tb where

  data BindCtx Var tb = VarCtx [Type tb]

  emptyCtx = VarCtx []

  bindTy b (VarCtx ts) =  Just $ ts !! (bindDepth b)

  addBinding t (VarCtx ts) = VarCtx (t:ts)
  addBindings ts varCtx = foldl (flip addBinding) varCtx ts

instance Binds' Var where
  bindDepth VZ     = 0
  bindDepth (VS n) = 1+bindDepth n

  buryBinding v 0 = v
  buryBinding v n = buryBinding (VS v) (n-1)

