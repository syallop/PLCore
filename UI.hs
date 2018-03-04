{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , ImplicitParams
  , FlexibleContexts
  #-}
module UI where

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
import PL.Reduce
import PL.Repl
import PL.Repl
import PL.TyVar
import PL.TyVar
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Var
import PLParser
import PLPrinter
import PLPrinter.Debug

import Control.Concurrent
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.List (isPrefixOf)
import Data.Text (Text)
import Graphics.Vty
import Lens.Micro
import Lens.Micro.TH
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import System.Directory (getAppUserDataDirectory)
import qualified Brick       as Brick
import qualified Brick.BChan as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Brick.Widgets.Border.Style as Brick
import qualified Brick.Widgets.Center as Brick
import qualified Data.Text as Text
import qualified System.Console.Haskeline.Brick as HB

-- | Non Vty events
data ReplEvent

  -- Haskeline event
  = HaskelineEv HB.ToBrick

  -- Haskeline threw an exception and is dead.
  | HaskelineDied (Either SomeException ())

  -- The ReplCtx is replaced with this value.
  | ReplCtxUpdated (ReplCtx Var TyVar)

-- | Name widgets, etc.
data ReplName
  -- Our entire app.
  = ReplApp

  -- The Haskeline command line input widget.
  | HaskelineWidget
  deriving (Eq, Ord, Show)

-- | The state of the repl.
data ReplState = ReplState
  {_replCtx         :: ReplCtx Var TyVar
  ,_haskelineWidget :: HB.Widget ReplName
  }
makeLenses ''ReplState

initialReplState :: ReplState
initialReplState = ReplState
  {_replCtx         = emptyReplCtx
  ,_haskelineWidget = HB.initialWidget HaskelineWidget
  }

type ReplApp = Brick.App ReplState ReplEvent ReplName

replApp :: HB.Config ReplEvent -> ReplApp
replApp c = Brick.App
  { Brick.appDraw         = drawUI
  , Brick.appChooseCursor = \_ -> Brick.showCursorNamed HaskelineWidget
  , Brick.appHandleEvent  = handleEvent c
  , Brick.appStartEvent   = pure
  , Brick.appAttrMap      = const replAttrMap
  }

-- | Match on events, updating the replstate accordingly.
handleEvent
  :: HB.Config ReplEvent
  -> ReplState
  -> Brick.BrickEvent ReplName ReplEvent
  -> Brick.EventM ReplName (Brick.Next ReplState)
handleEvent c (ReplState ctx hl) ev = do
  hl' <- HB.handleEvent c hl ev
  handleAppEvent (ReplState ctx hl') ev

handleAppEvent
  :: ReplState
  -> Brick.BrickEvent ReplName ReplEvent
  -> Brick.EventM ReplName (Brick.Next ReplState)
handleAppEvent (ReplState ctx hl) ev = case ev of
  Brick.AppEvent aEv
    -> case aEv of
         HaskelineDied e
           -> Brick.halt (ReplState ctx hl)

         ReplCtxUpdated ctx'
           -> Brick.continue (ReplState ctx' hl)

         _ -> Brick.continue (ReplState ctx hl)

  _ -> Brick.continue (ReplState ctx hl)

-- | Draw an instance of the repl.
drawUI
  :: ReplState
  -> [Brick.Widget ReplName]
drawUI replState =
  [HB.render (replState^.haskelineWidget)
  ]

-- | Map attribute names to attributes.
replAttrMap
  :: Brick.AttrMap
replAttrMap = Brick.attrMap defAttr
  [
  ]

runRepl :: IO ()
runRepl = do
  eventChan  <- Brick.newBChan 10
  config     <- HB.configure eventChan HaskelineEv (\ev -> case ev of
                                                     HaskelineEv e
                                                       -> Just e
                                                     _ -> Nothing
                                                   )
  _ <- forkFinally (runInput config eventChan) (Brick.writeBChan eventChan . HaskelineDied)
  void $ Brick.customMain (mkVty defaultConfig) (Just eventChan) (replApp config) initialReplState

runInput
  :: HB.Config ReplEvent
  -> Brick.BChan ReplEvent
  -> IO ()
runInput c eventChan = do
  hs <- haskelineSettings
  flip evalStateT emptyReplCtx . runInputTBehavior (HB.useBrick c) hs $ loop
  where
    myReplStep :: Text -> Repl Var (Type TyVar) TyVar Text
    myReplStep =
      let ?eb  = var
          ?abs = typ tyVar
          ?tb  = tyVar
       in replStep var (typ tyVar) tyVar

    loop :: InputT (StateT (ReplCtx Var TyVar) IO) ()
    loop = do
      mInput <- getInputLine "> "
      case mInput of
        Nothing
          -> return ()

        Just txt
          -> do replCtx0 <- lift get
                let (replCtx1,eRes) = (_unRepl $ myReplStep (Text.pack txt)) replCtx0
                case eRes of
                  Left err
                    -> do outputStrLn . Text.unpack . renderDocument $ err
                          loop

                  Right outputTxt
                    -> do -- outputStrLn . Text.unpack $ outputTxt
                          lift $ put replCtx1
                          liftIO $ Brick.writeBChan eventChan $ ReplCtxUpdated replCtx1
                          loop

haskelineSettings
  :: ( MonadState (ReplCtx Var TyVar) m
     , MonadIO m
     )
  => IO (Settings m)
haskelineSettings = do
  hf <- getAppUserDataDirectory "pl.history"
  pure Settings
    { historyFile    = Just hf
    , complete       = completer
    , autoAddHistory = True
    }
  where
    completer = completeWord Nothing [' ', '\t'] $ \w -> do
      s <- get
      return . map simpleCompletion . filter (isPrefixOf w) $ []

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \run ->
                    fmap (flip runStateT s) $ f $ stateRunIO s run
      where
        stateRunIO :: s -> RunIO m -> RunIO (StateT s m)
        stateRunIO s (RunIO run) = RunIO (\m -> fmap (StateT . const)
                                        $ run (runStateT m s))
