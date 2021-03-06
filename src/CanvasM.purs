module CanvasM where

import Prelude
import Effect (Effect)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask)
import Graphics.Canvas as GC

liftReaderT :: forall r m a. (Monad m) => m a -> ReaderT r m a
liftReaderT = lift

type CanvasM e a
  = ReaderT GC.Context2D Effect a

liftC :: forall a e.
  (GC.Context2D -> Effect a) -> CanvasM e Unit
liftC action = void $ ask >>= \ctx -> liftReaderT (action ctx)

runCanvasM :: forall a e.
  GC.Context2D -> CanvasM e a -> Effect a
runCanvasM = flip runReaderT

beginPath :: forall e. CanvasM e Unit
beginPath =
  liftC $ GC.beginPath

fillRect :: forall e. GC.Rectangle -> CanvasM e Unit
fillRect rect =
  liftC $ \ctx -> GC.fillRect ctx rect

translate :: forall e. GC.TranslateTransform -> CanvasM e Unit
translate tt =
  liftC $ \ctx -> GC.translate ctx tt

rotate :: forall e. Number -> CanvasM e Unit
rotate a =
  liftC $ \ctx -> GC.rotate ctx a

arc :: forall e. GC.Arc -> CanvasM e Unit
arc a =
  liftC $ \ctx -> GC.arc ctx a

moveTo :: forall e. Number -> Number -> CanvasM e Unit
moveTo x y =
  liftC $ \ctx -> GC.moveTo ctx x y

setFillStyle :: forall e. String -> CanvasM e Unit
setFillStyle style =
  liftC $ \ctx -> GC.setFillStyle ctx style

setStrokeStyle :: forall e. String -> CanvasM e Unit
setStrokeStyle style =
  liftC $ \ctx -> GC.setStrokeStyle ctx style

lineTo :: forall e. Number -> Number -> CanvasM e Unit
lineTo x y =
  liftC $ \ctx -> GC.lineTo ctx x y

withContext :: forall e. CanvasM e Unit -> CanvasM e Unit
withContext action =
  liftC $ \ctx -> GC.withContext ctx (runCanvasM ctx action)

stroke :: forall e. CanvasM e Unit
stroke =
  liftC GC.stroke

fill :: forall e. CanvasM e Unit
fill =
  liftC GC.fill

clearRect :: forall e. GC.Rectangle -> CanvasM e Unit
clearRect rect =
  liftC $ \ctx -> GC.clearRect ctx rect

fillText :: forall e. String -> Number -> Number -> CanvasM e Unit
fillText str x y =
  liftC $ \ctx -> GC.fillText ctx str x y

strokeText :: forall e. String -> Number -> Number -> CanvasM e Unit
strokeText str x y =
  liftC $ \ctx -> GC.strokeText ctx str x y

setTextAlign :: forall e. GC.TextAlign -> CanvasM e Unit
setTextAlign align =
  liftC $ \ctx -> GC.setTextAlign ctx align

setFont :: forall e. String -> CanvasM e Unit
setFont font =
  liftC $ \ctx -> GC.setFont ctx font

setLineWidth :: forall e. Number -> CanvasM e Unit
setLineWidth width =
  liftC $ \ctx -> GC.setLineWidth ctx width

setLineCap :: forall e. GC.LineCap -> CanvasM e Unit
setLineCap lc =
  liftC $ \ctx -> GC.setLineCap ctx lc
