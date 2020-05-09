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

import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Name
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

import PL.Name
import PLPrinter
import PLPrinter.Doc

import qualified Data.List.NonEmpty as NE
import qualified Data.List as List
import qualified Data.Text as Text

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
putGreen  = putColor 32
putRed    = putColor 31
putYellow = putColor 33
putBlue   = putColor 34
putCyan   = putColor 36

ppTypeName :: TypeName -> Doc
ppTypeName = PLPrinter.text . typeName

ppTermName :: TermName -> Doc
ppTermName = PLPrinter.text . termName

ppKind :: Kind -> Doc
ppKind k = case k of
  Kind
    -> PLPrinter.text "KIND"
  KindArrow from to
    -> PLPrinter.char '^' <> parens (ppKind from) <> parens (ppKind to)

