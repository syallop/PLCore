{-# LANGUAGE
    GADTs
  , OverloadedStrings
  #-}
module PL.Megaparsec where

import qualified PLGrammar as G
import PLGrammar.Iso
import PLPrinter

import qualified Data.Text as Text
import Data.Void
import Data.Text (Text)
import Control.Applicative
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Expr as Mega
import qualified Text.Megaparsec as Mega


-- | Convert a Grammar to a Parser that accepts it.
toParser :: G.Grammar a -> Mega.Parsec Void Text a
toParser grammar = case grammar of
  -- A single character if one is available.
  G.GAnyChar
    -> Mega.anyChar

  -- A token of text stopping at the first whitespace.
  G.GAnyText
    -> Text.pack <$> Mega.someTill Mega.anyChar Mega.spaceChar

  -- Return the value.
  G.GPure a
    -> pure a

  -- Fail with no Expectations.
  G.GEmpty
    -> empty

  -- If the left fails, try the right as if no input had been consumed.
  G.GAlt g0 g1
    -> toParser g0 <|> toParser g1

  -- Parse the grammar if the iso succeeds.
  G.GIsoMap iso ga
    -> isoMapParser iso ga

  -- | Tuple the result of two successive parsers.
  G.GProductMap ga gb
    -> productMapParser (toParser ga) (toParser gb)

  -- Enhance a failing parse with a given Expect label.
  G.GLabel l g
    -> toParser g Mega.<?> (Text.unpack $ renderDocument l)

  G.GTry g
    -> Mega.try $ toParser g

isoMapParser
  :: Show a
  => Iso a b
  -> G.Grammar a
  -> Mega.Parsec Void Text b
isoMapParser iso g = do
  a <- toParser g
  case parseIso iso a of
    Nothing
      -> fail $ Text.unpack $ Text.intercalate "." . _isoLabel $ iso
    Just b
      -> pure b

productMapParser
  :: Mega.Parsec Void Text a
  -> Mega.Parsec Void Text b
  -> Mega.Parsec Void Text (a, b)
productMapParser pl pr = (,) <$> pl <*> pr

