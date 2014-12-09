module BaseClient where

import Debug.Trace (Trace(), trace)
import Data.Maybe
import Data.Tuple (fst, snd)
import Data.Foldable (for_)
import qualified Data.Either as E
import Data.JSON (ToJSON, FromJSON, encode, eitherDecode)
import Control.Monad.State.Trans (StateT(), runStateT)
import Control.Monad.Writer.Trans (WriterT(), runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (newRef, readRef, writeRef, modifyRef, Ref(),
                              RefVal())
import Data.DOM.Simple.Types (DOM(), DOMEvent())
import Data.DOM.Simple.Events (addKeyboardEventListener, KeyboardEventType(..))
import Data.DOM.Simple.Window (globalWindow)
import Graphics.Canvas (Canvas())

import Types
import qualified BrowserWebSocket as WS

type Client st =
  { socket   :: WS.Socket
  , state    :: st
  , playerId :: PlayerId
  }

type ClientCallbacks st inc outg e =
  { onMessage     :: inc -> ClientM st outg e Unit
  , onKeyDown     :: DOMEvent -> ClientM st outg e Unit
  , render        :: PlayerId -> ClientM st outg e Unit
  }

type ClientM st outg e a =
  WriterT [outg] (StateT st (Eff (ClientEffects e))) a

type ClientMResult st outg a =
  { nextState :: st
  , messages  :: [outg]
  , result    :: a
  }

type ClientEffects e =
  (ref :: Ref, trace :: Trace, ws :: WS.WebSocket, dom :: DOM,
   canvas :: Canvas | e)

runClientM :: forall st outg e a.
  st -> ClientM st outg e a -> Eff (ClientEffects e) (ClientMResult st outg a)
runClientM state action = do
  let r1 = runWriterT action
  r2 <- runStateT r1 state
  return $
    { nextState: snd r2
    , messages: snd (fst r2)
    , result: fst (fst r2)
    }

mkClient :: forall st. st -> WS.Socket -> PlayerId -> Client st
mkClient initialState socket pId =
  { socket: socket
  , state: initialState
  , playerId: pId
  }

runCallback :: forall st outg args e. (ToJSON outg) =>
  RefVal (Client st) -> ClientM st outg e Unit -> Eff (ClientEffects e) Unit
runCallback refCln callback = do
  cln <- readRef refCln
  res <- runClientM cln.state callback
  sendAllMessages cln res.messages
  writeRef refCln $ cln { state = res.nextState }

sendAllMessages :: forall e st outg. (ToJSON outg) =>
  Client st -> [outg] -> Eff (ClientEffects e) Unit
sendAllMessages cln msgs =
  for_ msgs $ \msg ->
    WS.send cln.socket (encode msg)

sendUpdate :: forall m outg. (Monad m, MonadWriter [outg] m) =>
  outg -> m Unit
sendUpdate m = sendUpdates [m]

sendUpdates :: forall m outg. (Monad m, MonadWriter [outg] m) =>
  [outg] -> m Unit
sendUpdates = tell

startClient :: forall st inc outg e. (FromJSON inc, ToJSON outg) =>
  ClientCallbacks st inc outg e -> RefVal (Client st)
  -> Eff (ClientEffects e) Unit
startClient cs refCln = do
  cln <- readRef refCln
  WS.onMessage cln.socket $
    eitherDecode >>> E.either trace (runCallback refCln <<< cs.onMessage)

  addKeyboardEventListener
    KeydownEvent
    (runCallback refCln <<< cs.onKeyDown)
    globalWindow

  void $ startAnimationLoop $ do
    c <- readRef refCln
    runCallback refCln $ cs.render c.playerId


foreign import data AnimationLoop :: *

foreign import startAnimationLoop
  """
  function startAnimationLoop(action) {
    var loop = {}

    var go = (function() {
      window.requestAnimationFrame(this.go);
      action();
    }).bind(loop)
    loop.go = go

    var stop = (function() {
      this.go = function () { }
    }).bind(loop)
    loop.stop = stop

    return function() {
      loop.go()
      return loop
    }
  }
  """ :: forall a e. Eff e a -> Eff e AnimationLoop

foreign import stopAnimationLoop
  """
  function stopAnimationLoop(loop) {
    loop.stop()
  }
  """ :: forall e. AnimationLoop -> Eff e Unit
