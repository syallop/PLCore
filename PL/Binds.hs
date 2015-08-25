{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module PL.Binds where

import PL.Type

-- 'b' maps to and from an index describing where the variable was bound.
-- The associated ''BindCtx b' associates 'b' to types'
class (Show b,Eq b) => Binds b where
  ixBind    :: Int -> b
  bindIx    :: b -> Int
  addBindIx :: b -> Int -> b

  data BindCtx b

  emptyCtx :: BindCtx b
  bindTy   :: b -> BindCtx b -> Type
  addVar   :: Type -> BindCtx b -> BindCtx b
  addVars  :: [Type] -> BindCtx b -> BindCtx b

