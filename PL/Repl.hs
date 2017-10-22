{-# LANGUAGE
    RankNTypes
  , FlexibleContexts
  , OverloadedStrings
  #-}
module PL.Repl where

import PL.Abstracts
import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.ExprLike
import PL.Kind
import PL.Parser
import PL.Printer
import PL.Grammar
import PL.Grammar.Lispy
import PL.Name
import PL.Reduce
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.TypeCtx

import Control.Applicative
import Control.Monad (ap)
import Data.Maybe
import Data.List (intercalate)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | The current ctx of the repl is a consistent view of type and expression bindings.
data ReplCtx b tb = ReplCtx
  {_exprBindCtx  :: ExprBindCtx b tb -- Expr bindings 'b' have types
  ,_typeBindCtx  :: TypeBindCtx tb   -- Type bindings have kinds

  ,_typeBindings :: TypeBindings tb -- Type bindings may have a bound or unbound type

  ,_typeCtx      :: TypeCtx tb      -- Names can be given to types
  }

-- | An initial, empty replctx
emptyReplCtx
  :: (Binds b (Type tb)
     ,Binds tb Kind
     )
  => ReplCtx b tb
emptyReplCtx = ReplCtx
  {_exprBindCtx  = emptyCtx
  ,_typeBindCtx  = emptyCtx

  ,_typeBindings = emptyBindings

  ,_typeCtx     = mempty
  }

-- | A Repl has replctx as state which it always returns alongside a successful
-- result or an error.
--
-- This means, for example, we can update our state and throw an error at the
-- same time. One usecase would be tracking line numbers of entered expressions, valid or not.
newtype Repl b abs tb a = Repl
  {_unRepl :: ReplCtx b tb -> (ReplCtx b tb, Either (Error tb) a)}

instance Functor (Repl b abs tb) where
  fmap f (Repl r) = Repl $ \ctx -> let (ctx',res) = r ctx
                                    in (ctx', case res of
                                                Left err -> Left err
                                                Right a  -> Right $ f a)

instance Applicative (Repl b abs tb) where
  pure = return
  (<*>) = ap


instance Monad (Repl b abs tb) where
  return a = Repl $ \ctx -> (ctx,Right a)

  (Repl f) >>= fab = Repl $ \ctx -> let (ctx',res) = f ctx
                                     in case res of
                                          Left err -> (ctx',Left err)
                                          Right a  -> let Repl g = fab a
                                                         in g ctx'

-- | Inject an error into the repl
replError :: Error tb -> Repl b abs tb r
replError err = Repl $ \ctx -> (ctx,Left err)

-- Read text and parse it into an expr
replRead
  :: (Ord tb
     ,Eq b
     ,Eq abs
     ,Document b
     ,Document abs
     ,Document tb
     )
  => Text        -- ^ Input text
  -> Grammar b   -- ^ Expression bindings (E.G. Var)
  -> Grammar abs -- ^ Expression abstraction (E.G. Type)
  -> Grammar tb  -- ^ Type bindings (E.G. Var)
  -> Repl b abs tb (Expr b abs tb)
replRead input b abs tb = case runParser (toParser $ expr b abs tb) input of
  f@(ParseFailure expected cursor)
    -> replError . EMsg . render . document $ f

  s@(ParseSuccess expr cursor)
    | Text.null $ remainder cursor
     -> pure expr

    | otherwise
     -> replError $ EMsg $ render $ "Parse succeeded but there were trailing characters: " <> document cursor

-- Type check an expression in the repl context.
replTypeCheck
  :: (Abstracts abs tb
     ,Binds b (Type tb)
     ,Binds tb Kind
     ,Ord tb
     ,Document b
     ,Document abs
     ,Document tb
     )
  => Expr b abs tb
  -> Repl b abs tb (Type tb)
replTypeCheck expr = Repl $ \ctx ->
  case exprType (_exprBindCtx ctx)
                (_typeBindCtx ctx)
                (_typeBindings ctx)
                (_typeCtx ctx) expr of
    Left err
      -> (ctx,Left err)

    Right ty
      -> (ctx,Right ty)

-- Reduce an expression in the repl context.
replReduce
  :: (Binds b (Type tb)
     ,Abstracts abs tb
     ,Eq b
     )
  => Expr b abs tb
  -> Repl b abs tb (Expr b abs tb)
replReduce initialExpr = case reduce initialExpr of
  Left err   -> replError err
  Right expr -> pure expr

-- Type check and reduce an expression
replEval
  :: (Binds b (Type tb)
     ,Binds tb Kind
     ,Abstracts abs tb
     ,Eq b
     ,Ord tb
     ,Document b
     ,Document abs
     ,Document tb
     )
  => Expr b abs tb
  -> Repl b abs tb (Expr b abs tb,Type tb)
replEval expr = do
  ty      <- replTypeCheck expr
  redExpr <- replReduce    expr
  pure (redExpr,ty)

-- Transform a parsed expr, its reduction and type into some output to print
replPrint
  :: (Document b
     ,Document abs
     ,Document tb
     ,Implicits b abs tb
     ,Ord tb
     ,Eq b
     ,Eq abs
     )
  => (Expr b abs tb,Expr b abs tb,Type tb)
  -> Repl b abs tb Text
replPrint (inputExpr,redExpr,ty) = pure . renderDocument $
  ["input expression:",lineBreak
  , fromMaybe "" $ pprint (toPrinter exprI) inputExpr
  {-,document inputExpr,lineBreak-}

  ,"reduces to:",lineBreak
  ,document redExpr,lineBreak

  ,"with type:",lineBreak
  ,document ty,lineBreak
  ]

-- Produce the next context and parse, type-check and reduce or error.
replStep
  :: (Ord tb
     ,Document b
     ,Document abs
     ,Document tb
     ,Implicits b abs tb
     ,Binds b (Type tb)
     ,Binds tb Kind
     ,Abstracts abs tb
     ,Eq b
     )
  => Grammar b
  -> Grammar abs
  -> Grammar tb
  -> Text
  -> Repl b abs tb Text
replStep b abs tb txt = do
  expr         <- replRead txt b abs tb
  (redExpr,ty) <- replEval expr
  replPrint (expr,redExpr,ty)

