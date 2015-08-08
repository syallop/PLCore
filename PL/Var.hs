{-# LANGUAGE TypeFamilies #-}
module PL.Var where

import PL.Binds
import PL.Type

-- | Debrujn index for referencing variables.
-- A positive integer describing how many abstractions deep the var is found.
data Var = VZ | VS Var
  deriving (Show, Eq)

vzero  = VZ
vone   = VS vzero
vtwo   = VS vone
vthree = VS vtwo
vfour  = VS vthree

instance Binds Var where

  ixBind 0 = VZ
  ixBind n = VS (ixBind (n-1))

  bindIx VZ     = 0
  bindIx (VS v) = 1+bindIx v

  addBindIx b 0 = b
  addBindIx b n = addBindIx (VS b) (n-1)


  data BindCtx Var = VarCtx [Type]

  emptyCtx = VarCtx []

  bindTy b (VarCtx ts)  =  ts !! (bindIx b)

  addVar t (VarCtx ts) = VarCtx (t:ts)
  addVars ts varCtx = foldl (flip addVar) varCtx ts

