{-# LANGUAGE OverloadedStrings #-}
module PL.Parser.Lispy.Kind where

import Control.Applicative

import PL.Parser
import PL.Kind

kindAbs :: Parser Kind
kindAbs = kind

kind :: Parser Kind
kind = simpleKind <|> arrowKind

simpleKind :: Parser Kind
simpleKind = textIs "Kind" *> pure Kind

arrowKind :: Parser Kind
arrowKind = arrowise <$> (arrow *> kind) <*> kind <*> many kind
  where
    arrowise :: Kind -> Kind -> [Kind] -> Kind
    arrowise from to []     = KindArrow from to
    arrowise from to (t:ts) = KindArrow from (KindArrow to (arrowise to t ts))

