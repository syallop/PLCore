{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module PL.Abstracts where

import PL.Type

-- A type used in a lambda abstraction,
-- I.E. \ABS -> ...
class (Show abs,Eq abs,Show tb,Eq tb) => Abstracts abs tb where
  absTy :: abs -> Type tb

-- The simplest abstraction is a type with no additional data (like E.G. a variable name)
instance (Eq tb, Show tb) => Abstracts (Type tb) tb where
  absTy = id

