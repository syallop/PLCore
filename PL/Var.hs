module PL.Var where

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


-- | Debrujn indexed context of variable types
data VarCtx = VarCtx [Type]

-- | Initial empty context
emptyVarCtx :: VarCtx
emptyVarCtx = VarCtx []

-- | Index the type of a var
index :: Var -> VarCtx -> Type
index VZ     (VarCtx (l:_)) = l
index (VS v) (VarCtx (_:ls)) = index v (VarCtx ls)
index _ _ = error "VarCtx too small"

-- | Add a newly bound var to a context
addVar :: Type -> VarCtx -> VarCtx
addVar t (VarCtx ts) = VarCtx (t:ts)

-- | Add many newly bound vars to a context
addVars :: [Type] -> VarCtx -> VarCtx
addVars ts varCtx = foldl (flip addVar) varCtx ts

varToInt :: Var -> Int
varToInt VZ     = 0
varToInt (VS v) = 1+varToInt v

intToVar :: Int -> Var
intToVar 0 = VZ
intToVar n = VS (intToVar (n-1))

