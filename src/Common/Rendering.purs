module Rendering where

import Data.Array
import Data.Maybe
import Data.Foldable
import Graphics.Canvas
import Control.Monad.Eff

import ExtraDom
import LevelMap
import Types
import Utils

pxPerBlock :: Number
pxPerBlock = 2

-- the height and width of the canvas
canvasSize :: Number
canvasSize = pxPerBlock * LevelMap.mapSize

setupRendering :: forall e. Eff (canvas :: Canvas | e) Context2D
setupRendering = do
  c <- getCanvasElementById "canvas"
        >>= setCanvasHeight canvasSize
        >>= setCanvasWidth canvasSize
  getContext2D c

renderBackground :: forall e.
  Context2D -> Eff (canvas :: Canvas | e) Context2D
renderBackground ctx =
  let r = {x: 0, y: 0, h: canvasSize, w: canvasSize}
  in fillRect ctx r

-- render a row at the given y-coordinate on the canvas
renderRow :: forall e.
  Context2D
  -> [Block]
  -> Number
  -> Eff (canvas :: Canvas | e) Unit
renderRow ctx blocks y =
  eachWithIndex_ blocks $ \block n ->
    case block of
      Empty -> void $ fillRect ctx $ getRect n
      Wall -> return unit
  where
  getRect n = {x: n * pxPerBlock, y: y, h: pxPerBlock, w: pxPerBlock}

renderMap :: forall e.
  Context2D
  -> LevelMap
  -> Eff (canvas :: Canvas | e) Unit
renderMap ctx map =
  withContext ctx go
  where
  go = do
    setFillStyle "gray" ctx
    eachWithIndex_ (getBlocks map) $ \row n -> do
      renderRow ctx row (n * pxPerBlock)
      return unit

---

-- renderElems :: forall e a.
--   (a -> Eff (dom :: DOM | e) HTMLElement) -- rendering action
--   -> [a]                                  -- list of things to render
--   -> HTMLElement                          -- container
--   -> Eff (dom :: DOM | e) HTMLElement
-- renderElems f xs container = do
--   let acts = f <$> xs
--   renderedElems <- sequence acts
--   for_ renderedElems (appendChild container)
--   return container

-- renderInDiv :: forall e a.
--   String                                     -- class of container
--   -> (a -> Eff (dom :: DOM | e) HTMLElement) -- rendering action
--   -> [a]                                     -- list of things to render
--   -> Eff (dom :: DOM | e) HTMLElement
-- renderInDiv class_ f xs = do
--   c <- createElement "div"
--   classAdd class_ c
--   renderElems f xs c

-- renderMap :: forall e. LevelMap -> Eff (dom :: DOM | e) HTMLElement
-- renderMap (LevelMap blockRows) =
--   renderInDiv "levelmap" renderBlockRow blockRows

-- renderBlockRow :: forall e. [Block] -> Eff (dom :: DOM | e) HTMLElement
-- renderBlockRow blocks =
--   renderInDiv "row" renderBlock blocks

-- renderBlock :: forall e. Block -> Eff (dom :: DOM | e) HTMLElement
-- renderBlock b =
--   case b of
--     Wall -> go "wall"
--     Empty -> go "empty"
--   where
--   go extraClass = do
--     e <- createElement "div"
--     classAdd "block" e
--     classAdd extraClass e
--     return e
