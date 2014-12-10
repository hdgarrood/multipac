module HtmlViews where

import Data.Foldable (foldr, for_)
import Prelude hiding (id)
import Control.Lens ((~), (^.))
import Data.Tuple
import Data.String (replace, joinWith)
import Data.Array (sortBy, reverse, map)
import Data.Function (on)
import qualified Data.Map as M
import Text.Smolder.HTML
  (html, head, meta, script, style, body, div, h1, h2, canvas, title, link, p)
import Text.Smolder.HTML.Attributes
  (lang, charset, httpEquiv, content, src, defer, type', id, className, name,
  rel, href)
import Text.Smolder.Markup (text, (!), Markup())
import Text.Smolder.Renderer.String (render)

import Utils
import Types
import Style

replaceAll :: [Tuple String String] -> String -> String
replaceAll = fmap (uncurry replace) >>> foldr (>>>) Prelude.id

styles =
  replaceAll
    [ "${backgroundColor}" ~ backgroundColor
    , "${backgroundColorLighter}" ~ backgroundColorLighter
    , "${fontColor}" ~ fontColor
    , "${fontName}" ~ fontName
    , "${canvasSize}" ~ (show canvasSize)
    , "${canvasSize}" ~ (show canvasSize)
    ] $ rawStyles <> playerColorStyles

rawStyles = """
  .clearfix:after {
	visibility: hidden;
	display: block;
	font-size: 0;
	content: " ";
	clear: both;
	height: 0;
  }

  #background {
    position: absolute;
    z-index: 0;
  }

  #foreground {
    position: absolute;
    z-index: 1;
  }

  body {
    background-color: ${backgroundColor};
    color: ${fontColor};
    font: normal 100% ${fontName}, sans-serif;
  }

  h1 {
    text-align: center;
  }

  #game {
    margin: 0 auto;
    width: ${canvasSize}px;
  }

  #waiting-message {
    position: absolute;
    z-index: 2;
    width: ${canvasSize}px;
  }

  #waiting-message p {
    text-align: center;
    font-size: 140%;
    margin-left: 0 auto;
    margin-right: 0 auto;
  }

  .scores {
    margin-bottom: 5em;
  }

  .scores-header {
    font-size: 200%;
    text-align: center;
  }

  .scores-row {
    height: 2em;
    padding: 0.5em 1em 0.5em 1em;
    margin: 0 auto;
    width: 80%;
  }

  .scores-row.is-you {
    background-color: ${backgroundColorLighter};
  }

  .scores-cell {
    font-size: 150%;
    float: left;
  }

  .cell-wide {
    width: 60%;
  }

  .cell-thin {
    width: 20%;
  }

  .score {
    text-align: center;
  }
"""

playerColorStyles =
  concat $
    flip map allPlayerIds
      (\pId ->
        concat [".player-"
               , show pId
               , " { color: "
               , playerColor pId
               , ";}\n"
               ])
  where
  concat = joinWith ""


indexDoc =
  html ! lang "en" $ do
    head $ do
      meta ! charset "utf-8"
      meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
      script ! src "/js/game.js" ! defer "" $ text ""
      link ! rel "stylesheet" ! type' "text/css" ! href fontUrl
      style ! type' "text/css" $ text styles

    body $ do
      div ! id "game" $ do
        h1 $ text "multipac"
        div ! id "waiting-message" $ text ""
        canvas ! id "foreground" $ text ""
        canvas ! id "background" $ text ""

indexHtml = render indexDoc

waitingMessage :: ClientStateWaiting -> PlayerId -> String
waitingMessage sw pId = render $ waitingMessageDoc sw pId

waitingMessageDoc :: ClientStateWaiting -> PlayerId -> Markup
waitingMessageDoc sw pId = do
  whenJust sw.prevGame (scoresTable pId)
  let message =
      if sw ^. ready
         then "Waiting for other players..." ~ "ready: yes"
         else "Press SPACE when you're ready" ~ "ready: no"
  p $ text (fst message)
  p $ text (snd message)
  p $ text $ "You are: " <> show pId

scoresTable :: PlayerId -> Game -> Markup
scoresTable pId game =
  div ! className "scores clearfix" $ do
    h2 ! className "scores-header" $ text "scores"
    div ! className "scores-table" $ do
      let ps = M.toList (game ^. players)
      let sortedPs = reverse $ sortBy (compare `on` score) ps
      for_ sortedPs $ \(Tuple pId' p) -> do
        let cl = "scores-row" <> (if pId' == pId then " is-you" else "")
        div ! className cl $ do
          scoresCellDiv ["cell-thin", "player-" <> show pId'] (show pId')
          scoresCellDiv ["cell-wide"] "name here"
          scoresCellDiv ["cell-thin", "score"] (show (p ^. pScore))
  where
  score (Tuple _ p) = p ^. pScore
  scoresCellDiv classes innerText =
    div ! className (joinWith " " $ ["scores-cell"] <> classes) $
      text innerText

