{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Util
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Utility functions used when testing PL.
-}
module PL.Test.Util
  ( putColor
  , putGreen
  , putRed
  , putYellow
  , putBlue
  , putCyan

  , ppError
  , ppTypeName
  , ppTermName
  )
  where

import PL.Error

import PL.Name
import PLPrinter

-- Color a line of text
putColor
  :: Int
  -> String
  -> IO ()
putColor i s
  = putStr ("\x1b["++show i++"m")
 >> putStrLn s
 >> putStr "\x1b[0m"

-- Color a line of text. Newline
putGreen, putRed, putYellow, putBlue, putCyan :: String -> IO ()
putGreen  = putColor 32
putRed    = putColor 31
putYellow = putColor 33
putBlue   = putColor 34
putCyan   = putColor 36

ppTypeName :: TypeName -> Doc
ppTypeName = PLPrinter.text . typeName

ppTermName :: TermName -> Doc
ppTermName = PLPrinter.text . termName

