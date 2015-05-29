module PL.Var where

import PL.Type

-- | Debrujn index for referencing variables.
-- A positive integer describing how many abstractions deep the var is found.
data Var = VZ | VS Var
  deriving Show

-- | Debrujn indexed context of variable types
data VarCtx = VarCtx [Type]

-- | Initial empty context
emptyVarCtx :: VarCtx
emptyVarCtx = VarCtx []

-- | Index the type of a var
index :: Var -> VarCtx -> Type
index VZ     (VarCtx (l:ls)) = l
index (VS v) (VarCtx (_:ls)) = index v (VarCtx ls)

-- | Add a newly bound var to a context
addVar :: Type -> VarCtx -> VarCtx
addVar t (VarCtx ts) = VarCtx (t:ts)

-- | Add many newly bound vars to a context
addVars :: [Type] -> VarCtx -> VarCtx
addVars []     varCtx = varCtx
addVars (t:ts) varCtx = addVars ts $ addVar t varCtx

