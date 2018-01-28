{-# LANGUAGE
    RankNTypes
  , FlexibleContexts
  , OverloadedStrings
  , ImplicitParams
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
import PLParser
import PLPrinter
import PLPrinter.Debug
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
main = do
  hSetBuffering stdin LineBuffering
  repl emptyReplCtx
  where
    myReplStep :: Text -> Repl Var (Type TyVar) TyVar Text
    myReplStep =
      let ?eb = var
          ?abs = typ tyVar
          ?tb = tyVar
       in replStep var (typ tyVar) tyVar

    repl ctx = do
      putStr "PL> "
      hFlush stdout

      line <- Text.hGetLine stdin
      let (ctx', res) = (_unRepl $ myReplStep line) ctx
      case res of
        Left err
          -> Text.putStrLn . renderDocument $ err

        Right a
          -> Text.putStrLn a

      repl ctx'

