{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module PL.Binds where

import PL.Type

-- 'b' maps to and from an index describing where the variable was bound.
-- The associated ''BindCtx b' associates 'b' to types'
class (Show b,Eq b,Show abs,Eq abs) => Binds b abs | b -> abs, abs -> b where

  absTy :: abs -> Type


  ixBind    :: Int -> b
  bindIx    :: b -> Int
  addBindIx :: b -> Int -> b

  data BindCtx b

  emptyCtx :: BindCtx b
  bindTy   :: b -> BindCtx b -> Type
  addVar   :: Type -> BindCtx b -> BindCtx b
  addVars  :: [Type] -> BindCtx b -> BindCtx b
