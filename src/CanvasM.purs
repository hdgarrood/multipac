module CanvasM where
 
import Control.Monad.Eff (Eff())
import Control.Monad.Reader.Trans (ReaderT(), runReaderT, liftReaderT)
import Control.Monad.Reader.Class (ask)
import qualified Graphics.Canvas as GC

type CanvasM e a
  = ReaderT GC.Context2D (Eff (canvas :: GC.Canvas | e)) a

liftC :: forall a e.
  (GC.Context2D -> Eff (canvas :: GC.Canvas | e) a) -> CanvasM e Unit 
liftC action = void $ ask >>= \ctx -> liftReaderT (action ctx)

runCanvasM :: forall a e.
  GC.Context2D -> CanvasM e a -> Eff (canvas :: GC.Canvas | e) a
runCanvasM = flip runReaderT

beginPath :: forall e. CanvasM e Unit
beginPath =
  liftC $ GC.beginPath

fillRect :: forall e. GC.Rectangle -> CanvasM e Unit
fillRect rect =
  liftC $ \ctx -> GC.fillRect ctx rect

translate :: forall e. GC.TranslateTransform -> CanvasM e Unit
translate tt =
  liftC $ GC.translate tt

rotate :: forall e. Number -> CanvasM e Unit
rotate a =
  liftC $ GC.rotate a

arc :: forall e. GC.Arc -> CanvasM e Unit
arc a =
  liftC $ \ctx -> GC.arc ctx a

moveTo :: forall e. Number -> Number -> CanvasM e Unit
moveTo x y =
  liftC $ \ctx -> GC.moveTo ctx x y

setFillStyle :: forall e. String -> CanvasM e Unit
setFillStyle style =
  liftC $ GC.setFillStyle style

setStrokeStyle :: forall e. String -> CanvasM e Unit
setStrokeStyle style =
  liftC $ GC.setStrokeStyle style

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
  liftC $ \ctx -> GC.setFont font ctx

setLineWidth :: forall e. Number -> CanvasM e Unit
setLineWidth width =
  liftC $ \ctx -> GC.setLineWidth width ctx
