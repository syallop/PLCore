{-# LANGUAGE
    FlexibleInstances
  , OverloadedStrings
  , RankNTypes
  , UndecidableInstances
  #-}
{-|
Module      : PL.Printer
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A NIH Pretty-Printer
-}
module PL.Printer
  (-- * Types
   Doc(..)
  ,DocFmt()

  -- * Render a Doc
  ,mkDocFmt
  ,docFmt
  ,render
  ,renderWith

  -- * Create Docs
  -- ** From basic text
  ,char
  ,text
  ,string

  ,usingShow

  -- ** Indentation
  ,indent
  ,indent1

  -- ** From primitive types
  ,int
  ,bool

  -- **
  ,between
  ,parens
  ,emptyDoc

  ,lineBreak
  ,newLine

  -- * Class of things which have a canonical Doc
  , Document
  , document
  , renderDocument
  )
  where

import Data.List
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text

-- | A 'Doc'ument is something that can be printed nicely and appended to
-- efficiently.
data Doc
  = DocText Text      -- ^ Literal text
  | DocIndent Int Doc -- ^ Indent a Doc by an additional amount
  | DocAppend Doc Doc -- ^ Append two documents
  | DocEmpty          -- ^ Empty document
  | DocBreak          -- ^ Line break

data DocFmt = DocFmt
  {_lineLength  :: Int -- maximum total length of a line of text before a new line is inserted
  ,_indent      :: Int -- number of spaces inserted at the start of a line of text
  ,_colPosition :: Int -- the current position within a line. Always greater than the indent and less than the line length.
                       -- Used when appending to remember how far into the current line we are
  }

instance Monoid Doc where
  mempty  = DocEmpty

  mappend DocEmpty y        = y
  mappend x        DocEmpty = x
  mappend x        y        = DocAppend x y

docFmt :: DocFmt
docFmt = DocFmt 80 0 0

mkDocFmt :: Int -> DocFmt
mkDocFmt lineLength = DocFmt lineLength 0 0

-- | Render a document to Text with some 'DocFmt' formatting settings.
renderWith :: DocFmt -> Doc -> Text
renderWith fmt doc = fst $ renderWith' fmt doc
  where
    renderWith' :: DocFmt -> Doc -> (Text,DocFmt)
    renderWith' fmt d = case d of
      DocText txt
        -> let (endFirstLine,rest) = Text.splitAt (remainingLineLength fmt) txt

               -- Is the first line filled?
               filledFirstLine     = Text.length endFirstLine == _lineLength fmt

               -- Starting from the next new line, a list of text chunks with the correct amount
               -- of indent spaces ahead of them
               restChunks          = (indentSpaces fmt :)
                                   . intersperse (indentSpaces fmt)
                                   . Text.chunksOf (maximumUsableLength fmt)
                                   $ rest

               -- The amount of characters remaining on the last line filled is either the length of the last line
               -- , or if there isnt one, we're still on the same line and so its the original position plus however much we added
               lastLineRemaining   = maybe (_colPosition fmt + Text.length endFirstLine)
                                           Text.length
                                   . safeHead
                                   . reverse
                                   $ restChunks

               -- If we didnt fill the first line, then no new line is required and the 'restChunks' must be empty.
               resultText          = endFirstLine
                                  <> if filledFirstLine
                                       then "\n" <> Text.intercalate "\n" restChunks
                                       else ""
              in (resultText,fmt{_colPosition = lastLineRemaining})

      DocIndent i d
        -> let newIndent   = _indent fmt + i
               newFmt      = fmt{_colPosition = newIndent
                                ,_indent      = newIndent
                                }
               txt0        = "\n" <> indentSpaces newFmt
               (txt1,rFmt) = renderWith' newFmt d
              in (txt0<>txt1,rFmt)

      DocEmpty
        -> (Text.empty,fmt)

      -- Append two Docs with either a space between them or a break to a new line
      DocAppend d0 d1
        -> let (txt0,fmt0) = renderWith' fmt d0

               -- If theres no room left on the line, start a new one for the second append.
               -- Otherwise add a space before the second append
               (txt1,fmt1) = if remainingLineLength fmt0 == 0
                               then ("\n"<>indentSpaces fmt0,fmt0{_colPosition = _indent fmt0})
                               else (" ",fmt0{_colPosition = _colPosition fmt0 + 1})

               (txt2,fmt2) = renderWith' fmt1 d1
              in (txt0<>txt1<>txt2,fmt2)

      DocBreak
        -> ("\n" <> indentSpaces fmt,fmt{_colPosition = _indent fmt})

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- The amount of usable space left before the end of the line
remainingLineLength :: DocFmt -> Int
remainingLineLength fmt = _lineLength fmt - _indent fmt

-- The amount of usable characters in a line.
-- The maximum lines length subtract the number of spaces in the indentation.
maximumUsableLength :: DocFmt -> Int
maximumUsableLength fmt = _lineLength fmt - _colPosition fmt

-- A string of spaces for the current indentation level
indentSpaces :: DocFmt -> Text
indentSpaces fmt = Text.replicate (_indent fmt) " "

-- | Render a 'Doc'ument to Text with default formatting settings.
render :: Doc -> Text
render = renderWith docFmt

-- Print a single char
char :: Char -> Doc
char = DocText . Text.singleton

-- Print some text.
text :: Text -> Doc
text = DocText

string :: String -> Doc
string = text . Text.pack

-- Use a things show instance as input to DocText
usingShow :: Show a => a -> Doc
usingShow = string . show

bool :: Bool -> Doc
bool = usingShow

int :: Int -> Doc
int = usingShow


{-listUsing :: (a -> Doc) -> Doc -> Doc -> Doc -> [a] -> Doc-}
{-listUsing elemF open sep close l = open-}
                                {-<> (intersperse sep . map elemF $ l)-}
                                {-<> close-}

-- Indent a document by a quantity
indent :: Int -> Doc -> Doc
indent = DocIndent

-- Indent a document by a single space
indent1 :: Doc -> Doc
indent1 = DocIndent 1

lineBreak :: Doc
lineBreak = DocBreak

newLine :: Doc -> Doc
newLine d = d <> lineBreak

between :: (Doc,Doc) -> Doc -> Doc
between (o,c) d = o <> d <> c

parens :: Doc -> Doc
parens = between (char '(',char ')')

emptyDoc :: Doc
emptyDoc = DocEmpty

class Document d where
  document :: d -> Doc

renderDocument :: Document d => d -> Text
renderDocument = render . document

instance Document ()     where document () = text "()"
instance Document Char   where document = char
instance Document Text   where document = text
instance Document String where document = string
instance Document Bool   where document = bool
instance Document Int    where document = int
instance Document Doc    where document = id
instance Document [Doc]  where document = foldr DocAppend DocEmpty

instance IsString Doc where
  fromString = string

