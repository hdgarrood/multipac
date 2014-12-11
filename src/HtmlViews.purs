module HtmlViews where

import Data.Foldable (foldr, for_)
import Prelude hiding (id)
import Control.Lens ((~), (^.))
import Control.Monad (when)
import Data.Tuple
import Data.Maybe
import Data.String (replace, joinWith)
import Data.Array (sortBy, reverse, map, catMaybes)
import Data.Function (on)
import qualified Data.Map as M
import Text.Smolder.HTML
  (html, head, meta, script, style, body, div, h1, h2, canvas, title, link, p,
   input)
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
    , "${fontColor}" ~ fontColor
    , "${fontColor}" ~ fontColor
    , "${fontName}" ~ fontName
    , "${canvasSize}" ~ (show canvasSize)
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

  body {
    background-color: ${backgroundColor};
    color: ${fontColor};
    font: normal 100% ${fontName}, sans-serif;
  }

  h1 {
    text-align: center;
  }

  div, p {
    word-wrap: break-word;
  }

  input {
    border: 5px solid ${fontColor};
    -webkit-box-shadow:
      inset 0 0 8px  rgba(0,0,0,0.1),
            0 0 16px rgba(0,0,0,0.1);
    -moz-box-shadow:
      inset 0 0 8px  rgba(0,0,0,0.1),
            0 0 16px rgba(0,0,0,0.1);
     box-shadow:
      inset 0 0 8px  rgba(0,0,0,0.1),
            0 0 16px rgba(0,0,0,0.1);
    padding: 10px;
    background: rgba(255,255,255,0.3);
    margin: 0 0 10px 0;
    font-size: 150%;
    color: ${fontColor};
    width: 300px;
  }

  #game {
    margin: 0 auto;
    width: ${canvasSize}px;
  }

  #background {
    position: absolute;
    z-index: 0;
  }

  #foreground {
    position: absolute;
    z-index: 1;
  }

  #scores-container {
    position: absolute;
    z-index: 2;
    width: ${canvasSize}px;
  }

  #waiting-message {
    position: absolute;
    z-index: 3;
    width: ${canvasSize}px;
  }

  #prompt {
    position: absolute;
    z-index: 3;
    margin: 0 auto;
    text-align: center;
    width: ${canvasSize}px;
    font-size: 145%;
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

  .simple-scores {
    width: 80%;
    margin-left: 10px;
  }

  .simple-scores .player-P1,
  .simple-scores .player-P2,
  .simple-scores .player-P3,
  .simple-scores .player-P4 {
    text-align: right;
  }

  .is-you {
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

  .cell-thinnest {
    width: 11%;
  }

  .score {
    text-align: center;
  }

  .ready-state {
    width: 25%;
    float: left;
  }
"""

playerColorStyles =
  concat $
    flip map allPlayerIds
      (\pId ->
        concat [".player-"
               , show pId
               , " { color: "
               , playerColor pId "100%"
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

        div ! id "prompt" $ do
          p ! id "prompt-message" $ text ""
          input ! id "prompt-input" ! type' "text"

        div ! id "waiting-message" $ text ""

        canvas ! id "foreground" $ text ""
        canvas ! id "background" $ text ""

        div ! id "scores-container" $ text ""

indexHtml = render indexDoc

type PlayerReadyInfo
  = { ready :: Boolean
    , name  :: String
    }

waitingMessage ::
  ClientStateWaiting -> PlayerId -> M.Map PlayerId String -> String
waitingMessage sw pId ps = render $ waitingMessageDoc sw pId ps

waitingMessageDoc ::
  ClientStateWaiting -> PlayerId -> M.Map PlayerId String -> Markup
waitingMessageDoc sw pId playersMap = do
  whenJust sw.prevGame (scoresTable pId playersMap)

  let r = fromMaybe false $ M.lookup pId sw.readyStates
  p $ text $ if r
               then "Waiting for other players..."
               else "Press SPACE when you're ready"

  div ! className "clearfix" $
    for_ allPlayerIds $ \pId' -> do
      let mInfo = getPlayerInfo pId'
      let cl = "ready-state" <>
                    (if pId' == pId then " is-you" else "") <>
                    (if isJust mInfo then "" else " not-connected")

      div ! className cl $ do
        let cl' = "player-" <> show pId'
        p ! className cl' $ text (show pId')
        whenJust mInfo $ \info -> do
          p $ text info.name
          p $ text $ if info.ready then "ready!" else "not ready"
  where
  getPlayerInfo pId'' = do
    ready <- M.lookup pId'' sw.readyStates
    name <- M.lookup pId'' playersMap
    return $ { ready: ready, name: name }

type PlayerScoreInfo
  = { score :: Number
    , name  :: String
    , pId   :: PlayerId
    }

scoresTable :: PlayerId -> M.Map PlayerId String -> Game -> Markup
scoresTable pId playersMap game =
  div ! className "scores clearfix" $ do
    h2 ! className "scores-header" $ text "scores"
    div ! className "scores-table" $ do
      for_ (sortedPlayerInfos game) $ \info -> do
        let cl = "scores-row" <> (if info.pId == pId then " is-you" else "")
        div ! className cl $ do
          scoresCellDiv ["cell-thin", "player-" <> show info.pId] (show info.pId)
          scoresCellDiv ["cell-wide"] info.name
          scoresCellDiv ["cell-thin", "score"] (show info.score)
  where
  sortedPlayerInfos game =
    reverse $ sortBy (compare `on` score) (playerInfos game playersMap)

  score i = i.score

  scoresCellDiv classes innerText =
    div ! className (joinWith " " $ ["scores-cell"] <> classes) $
      text innerText


playerInfos game playersMap =
  catMaybes $ map (getPlayerInfo playersMap) allPlayers
  where
  allPlayers = M.toList (game ^. players)


getPlayerInfo ::
  M.Map PlayerId String -> Tuple PlayerId Player -> Maybe PlayerScoreInfo
getPlayerInfo playersMap (Tuple pId'' p) = do
  name <- M.lookup pId'' playersMap
  return $ { score: p ^. pScore, name: name, pId: pId'' }


simpleScores :: M.Map PlayerId String -> Game -> String
simpleScores m game =
  render $ simpleScoresMarkup m game

simpleScoresMarkup :: M.Map PlayerId String -> Game -> Markup
simpleScoresMarkup playersMap game = do
  div ! className "simple-scores" $ do
    for_ (playerInfos game playersMap) $ \info -> do
      scoresCellDiv ["cell-thinnest", "player-" <> show info.pId] (show info.pId)
      scoresCellDiv ["cell-thinnest", "score"] (show info.score)

  where
  scoresCellDiv classes innerText =
    div ! className (joinWith " " $ ["scores-cell"] <> classes) $
      text innerText
