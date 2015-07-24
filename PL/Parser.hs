{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module PL.Parser
  (-- Core parser functions
   Parser()
  ,runParser
  ,pFail
  ,pSucceed
  ,req
  ,sat
  ,alternatives

   -- Functions on characters
  ,takeChar
  ,takeCharIf
  ,charIs

  -- Take kinds of character
  ,upper
  ,lower
  ,digit

  -- Match kinds of character
  ,space
  ,arrow
  ,bar
  ,star
  ,plus
  ,comma
  ,up
  ,lambda
  ,langle
  ,rangle
  ,lparen
  ,rparen
  ,underscore

  -- Functions on Text/ many characters
  ,takeN
  ,takeNIf
  ,textIs
  ,takeWhile
  ,takeWhile1

  -- Misc
  ,natural
  ,between
  ,betweenParens
  ) where

import Prelude hiding (takeWhile,exp)

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Char

import PL.Name
import PL.Var
import PL.Type
import PL.Expr

-- | A Parser is a function which takes 'Text' and either fails or produces some 'a' and some leftover 'Text'.
-- Instances for Monad & Applicative sequence Parsers together left-to-right, propogating failure.
-- Instances for MonadPlus & Alternative sequence left-to-right when successful but have backtracking behaviour on failure.
newtype Parser a = Parser {_unParser :: Text -> Maybe (a,Text)}

instance Monoid a => Monoid (Parser a) where
  mempty = return mempty
  mappend pa pa' = do
    a  <- pa
    a' <- pa'
    return (a <> a')

instance Functor Parser where
  fmap f (Parser pa) = Parser $ \txt -> case pa txt of
    Nothing       -> Nothing
    Just (a,txt') -> Just (f a,txt')


instance Applicative Parser where
  pure  = return
  (<*>) = ap
instance Monad Parser where
  return a = Parser $ \txt -> Just (a,txt)

  (Parser pa) >>= f = Parser $ \txt -> case pa txt of
    Nothing       -> Nothing
    Just (a,txt') -> let Parser pb = f a in pb txt'

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus
instance MonadPlus Parser where
  mzero = Parser $ \txt -> Nothing

  mplus (Parser pa0) (Parser pa1) = Parser $ \txt -> case pa0 txt of
    Nothing        -> pa1 txt
    Just (a0,txt') -> Just (a0,txt')

-- | Execute a 'Parser' on some input Text, producing a possible result and leftover Text if successful.
runParser :: Parser a -> Text -> Maybe (a,Text)
runParser (Parser p) txt = p txt


-- | Fail without consuming anything
pFail :: Parser a
pFail = Parser $ const Nothing

-- | Succeed without consuming anything
pSucceed :: Parser ()
pSucceed = Parser $ \txt -> Just ((),txt)

-- | Require a parse must succeed, but any result is discarded
req :: Parser a -> Parser ()
req p = p >>= \_ -> pSucceed

-- | A parse must suceed and satisfy a predicate.
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred p = p >>= \a -> if pred a then return a else pFail

-- | Try each parser in succession, backtracking on failure.
alternatives :: [Parser a] -> Parser a
alternatives []     = pFail
alternatives (p:ps) = p <|> alternatives ps



-- Take a single character (if there are any left that is..)
takeChar :: Parser Char
takeChar = Parser $ Text.uncons

-- Take a character that must satisfy a predicate
takeCharIf :: (Char -> Bool) -> Parser Char
takeCharIf pred = sat pred takeChar

-- Take a character if it is equal to the one given
charIs :: Char -> Parser ()
charIs c = req $ takeCharIf (== c)




upper = takeCharIf isUpper :: Parser Char
lower = takeCharIf isLower :: Parser Char
digit = takeCharIf isDigit :: Parser Char


space      = req $ takeCharIf isSpace
arrow      = textIs "->"
bar        = charIs '|'
star       = charIs '*'
plus       = charIs '+'
comma      = charIs ','
up         = charIs '^'
lambda     = charIs '\\'
langle     = charIs '<'
rangle     = charIs '>'
lparen     = charIs '('
rparen     = charIs ')'
underscore = charIs '_'



-- | Take a number of chars, if the input is long enough
takeN :: Int -> Parser Text
takeN i
  | i < 0     = error "Cant take a negative number of characters"
  | i == 0    = return ""
  | otherwise = Parser $ \txt -> case Text.compareLength txt i of
      LT -> Nothing
      _  -> Just $ Text.splitAt i txt

-- Take a number of chars if the resulting text passes a predicate
takeNIf :: (Text -> Bool) -> Int -> Parser Text
takeNIf pred i = sat pred $ takeN i

-- Take a string of text
textIs :: Text -> Parser ()
textIs t = req $ takeNIf (== t) (Text.length t)


-- Take the longest text that matches a predicate on the characters
-- (possibly empty)
takeWhile :: (Char -> Bool) -> Parser Text
takeWhile pred = Parser $ Just . Text.span pred

-- Takewhile, but must take at least one character => not the empty text
takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 pred = Parser $ \txt -> case Text.span pred txt of
  ("",_) -> Nothing
  r      -> Just r




-- A natural number: zero and positive integers
natural :: Parser Int
natural = read . Text.unpack <$> takeWhile1 isDigit

-- | Discard the result of two wrapping parsers.
between :: Parser l -> Parser a -> Parser r -> Parser a
between pl pa pr = pl *> pa <* pr

-- | Run a parser between two parenthesis '(' ')'.
betweenParens :: Parser a -> Parser a
betweenParens pa = between lparen pa rparen

