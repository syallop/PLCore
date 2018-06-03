{-# LANGUAGE
    GADTs
  , OverloadedStrings
  #-}
module PL.Megaparsec where

import qualified PLGrammar as G
import PLPrinter
import Reversible
import Reversible.Iso

import qualified Data.Text as Text
import Data.Void
import Data.Text (Text)
import Control.Applicative
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Expr as Mega
import qualified Text.Megaparsec as Mega


-- | Convert a Grammar to a Parser that accepts it.
toParser :: G.Grammar a -> Mega.Parsec Void Text a
toParser (Reversible r) = case r of
  ReversibleInstr i
    -> case i of
         -- A single character if one is available.
         G.GAnyChar
           -> Mega.anyChar

         -- Enhance a failing parse with a given Expect label.
         G.GLabel l g
           -> toParser g Mega.<?> (Text.unpack $ renderDocument l)

         G.GTry g
           -> Mega.try $ toParser g

  -- Return the value.
  RPure a
    -> pure a

  -- Fail with no Expectations.
  REmpty
    -> empty

  -- If the left fails, try the right as if no input had been consumed.
  RAlt g0 g1
    -> toParser g0 <|> toParser g1

  -- Parse the grammar if the iso succeeds.
  RMap iso ga
    -> rmapParser iso ga

  -- | Tuple the result of two successive parsers.
  RAp ga gb
    -> rapParser (toParser ga) (toParser gb)

rmapParser
  :: Show a
  => Iso a b
  -> G.Grammar a
  -> Mega.Parsec Void Text b
rmapParser iso g = do
  a <- toParser g
  case forwards iso a of
    Nothing
      -> fail "iso"
    Just b
      -> pure b

rapParser
  :: Mega.Parsec Void Text a
  -> Mega.Parsec Void Text b
  -> Mega.Parsec Void Text (a, b)
rapParser pl pr = (,) <$> pl <*> pr

