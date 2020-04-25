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
  , ppCursor
  , ppExpected
  , ppPos
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
import PLParser
import PLPrinter
import PLPrinter.Doc
import PLParser.Cursor

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
ppTypeName (TypeName n) = PLPrinter.char '#' <> PLPrinter.text n

ppTermName :: TermName -> Doc
ppTermName (TermName n) = PLPrinter.char '#' <> PLPrinter.text n

ppKind :: Kind -> Doc
ppKind k = case k of
  Kind
    -> PLPrinter.text "KIND"
  KindArrow from to
    -> PLPrinter.char '^' <> parens (ppKind from) <> parens (ppKind to)

ppCursor :: Cursor -> Doc
ppCursor (Cursor prev next pos) =
  let (before,pointer,after) = point (Cursor prev next pos)
   in mconcat [ rawText before
              , lineBreak
              , text pointer
              , lineBreak
              , ppPos pos
              , lineBreak

              , rawText after
              ]

ppPos :: Pos -> Doc
ppPos (Pos t l c) = mconcat
  [text "Line:     ", int l,lineBreak
  ,text "Character:", int c,lineBreak
  ,text "Total:    ", int t,lineBreak
  ]

ppExpected :: Expected -> Doc
ppExpected e = showExpectedDoc e

showExpectedDoc :: Expected -> Doc
showExpectedDoc = bulleted
                . flattenExpectedDoc


-- Returns alternatives
flattenExpectedDoc :: Expected -> [Doc]
flattenExpectedDoc e = List.nub $ case e of
  ExpectEither es0 es1
    -> flattenExpectedDoc es0 <> flattenExpectedDoc es1

  ExpectFail
    -> []

  ExpectText txt
    -> [text txt]

  -- For a predicate with a descriptive label, the label is enough.
  ExpectPredicate (Label lTxt Descriptive) _
    -> [text $ "_PREDICATE_" <> lTxt]

  -- For an enhancing label, we still want to see the rest of the definition.
  ExpectPredicate (Label lTxt Enhancing) mE
    -> map ((text "_PREDICATE_" <> text lTxt) <>) $ maybe [] flattenExpectedDoc mE

  ExpectAnything
    -> [text "ANYTHING"]

  ExpectN i e
    -> [text $ "_EXACTLY_" <> (Text.pack . show $ i) <> "_"
       ,mconcat . flattenExpectedDoc $ e
       ]

  -- A descriptive label is sufficient.
  ExpectLabel (Label lTxt Descriptive) e
    -> [text lTxt]

  -- An enhancing label requires the rest of the definition.
  ExpectLabel (Label lTxt Enhancing) e
    -> [text $ lTxt <> " " <> (render . mconcat . flattenExpectedDoc $ e)
       ]

  ExpectThen e0 e1
    -> [ text . render . mconcat . flattenExpectedDoc $ e0
       , text "_THEN_"
       , text . render . mconcat . flattenExpectedDoc $ e1
       ]

