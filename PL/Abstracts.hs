{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           #-}
{-|
Module      : PL.Abstracts
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Things which can be used like the abstraction in a lambda. I.E. \ABS -> ...
-}
module PL.Abstracts where

import PL.Type

-- A type used in a lambda abstraction,
-- I.E. \ABS -> ...
class Abstracts abs tb where
  absTy :: abs -> Type tb

-- The simplest abstraction is a type with no additional data (like E.G. a variable name)
instance Abstracts (Type tb) tb where
  absTy = id

