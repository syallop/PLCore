{-# LANGUAGE
    RankNTypes
  , FlexibleContexts
  , OverloadedStrings
  #-}
module Main where

import PL.Abstracts
import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.ExprLike
import PL.Grammar.Lispy
import PL.Kind
import PL.Name
import PL.Parser
import PL.Printer
import PL.Printer.Debug
import PL.Reduce
import PL.Repl
import PL.TyVar
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import Control.Applicative
import Control.Monad (ap)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.IO

import Data.Text (Text)
import qualified Data.Text.IO as Text

main :: IO ()
main = repl emptyReplCtx
  where
    myReplStep = replStep var (typ tyVar) tyVar

    repl ctx = do
      putStr "PL> "
      hFlush stdout

      line <- Text.getLine
      let (ctx', res) = (_unRepl $ myReplStep line) ctx
      case res of
        Left err
          -> Text.putStrLn . renderDocument $ err

        Right a
          -> Text.putStrLn a

      repl ctx'

