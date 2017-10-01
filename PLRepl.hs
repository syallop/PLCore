{-# LANGUAGE
    RankNTypes
  , FlexibleContexts
  , OverloadedStrings
  #-}
module Main where

import PL.Repl
import PL.Abstracts
import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.ExprLike
import PL.Kind
import PL.Parser
import PL.Name
import PL.Reduce
import PL.Printer
import PL.Printer.Debug
import PL.TyVar
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.TypeCtx
import PL.Parser.Lispy
import PL.Var

import Control.Applicative
import Control.Monad (ap)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Text (Text)
import qualified Data.Text.IO as Text

main :: IO ()
main = repl emptyReplCtx
  where
    myReplStep = replStep var (typ tyVar) tyVar

    repl ctx = do
      line <- Text.getLine
      let (ctx', res) = (_unRepl $ myReplStep line) ctx
      case res of
        Left err
          -> Text.putStrLn . renderDocument $ err

        Right a
          -> Text.putStrLn a

      repl ctx'

