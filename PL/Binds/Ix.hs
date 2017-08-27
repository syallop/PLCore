{-|
Module      : PL.Binds.Ix
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.Binds.Ix where

-- class of index-like binding types, E.G. Var.
-- A binding type maps to an index describing where the abstraction it binds is and may be buries below a number of new abstractions
class BindingIx b where
  -- How far away is the abstraction we're binding?
  bindDepth :: b -> Int

  -- During a reduction, a binding may be buried below a number of new abstractions
  buryBinding :: b -> Int -> b

