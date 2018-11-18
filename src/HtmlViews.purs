module HtmlViews where

import Prelude hiding (div)
import Data.Foldable (foldr, for_)
import Data.Lens.Getter ((^.))
import Control.Monad (when)
import Data.Tuple
import Data.Tuple.Nested ((/\))
import Data.Maybe
import Data.String as String
import Data.Array (sortBy, reverse, catMaybes)
import Data.List as List
import Data.Function (on)
import Data.Map as M
import Text.Smolder.HTML
  (html, head, meta, script, style, body, div, h1, h2, canvas, title, link, p,
   input, a)
import Text.Smolder.HTML.Attributes
  (lang, charset, httpEquiv, content, src, defer, type', id, className, name,
  rel, href)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.HTML (Html)

import Utils
import Types
import Style

replaceAll :: Array (Tuple String String) -> String -> String
replaceAll =
  map (\(var /\ value) -> String.replace (String.Pattern var) (String.Replacement value)) >>>
  foldr (>>>) identity

styles =
  -- TODO: this is a bit silly. purescript-css?
  replaceAll
    [ "${linkColor}" /\ linkColor
    , "${backgroundColor}" /\ backgroundColor
    , "${backgroundColor}" /\ backgroundColor
    , "${backgroundColorLighter}" /\ backgroundColorLighter
    , "${fontColor}" /\ fontColor
    , "${fontColor}" /\ fontColor
    , "${fontColor}" /\ fontColor
    , "${fontName}" /\ fontName
    , "${tileColor}" /\ tileColor
    , "${canvasSize}" /\ (show canvasSize)
    , "${canvasSize}" /\ (show canvasSize)
    , "${canvasSize}" /\ (show canvasSize)
    , "${canvasSize}" /\ (show canvasSize)
    , "${canvasSize}" /\ (show canvasSize)
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
    margin: 0;
  }

  h1 {
    padding-top: 1em;
    text-align: center;
  }

  div, p {
    word-wrap: break-word;
  }

  a, a:visited {
    color: ${linkColor};
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

  #error {
    position: fixed;
    top: 25%;
    left: 50%;
    transform: translate(-50%, -50%);
    z-index: 101;

    width: ${canvasSize}px;
    background-color: ${backgroundColor};
    display: none;

    border: 1px solid;
    border-radius: 10px;
  }

  #error-overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    display: none;
    z-index: 100;
    background-color: rgba(0,0,0, 0.5);
  }

  #error h2 {
    text-align: center;
  }

  #error p {
    padding: 0 4em 0 4em;
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

playerColorStyles :: String
playerColorStyles =
  concat $
    flip map allPlayerIds
      (\pId ->
        concat [".player-"
               , displayPlayerId pId
               , " { color: "
               , playerColor pId
               , ";}\n"
               ])
  where
  concat = String.joinWith ""

indexHtml :: forall void. Html void
indexHtml =
  html ! lang "en" $ do
    head $ do
      meta ! charset "utf-8"
      meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
      script ! src "/js/client.js" ! defer "" $ text ""
      link ! rel "stylesheet" ! type' "text/css" ! href fontUrl
      style ! type' "text/css" $ text styles

    body $ do
      div ! id "error-overlay" $ text ""
      div ! id "game" $ do
        h1 $ text "multipac"

        div ! id "prompt" $ do
          p ! id "prompt-message" $ text ""
          input ! id "prompt-input" ! type' "text"

        div ! id "waiting-message" $ text ""

        canvas ! id "foreground" $ text ""
        canvas ! id "background" $ text ""

        div ! id "error" $ do
          h2 $ text "disconnected"
          p $ do
            text "Something went wrong and the server couldn't be contacted. "
            text "Please "
            a ! href "" $ text "refresh the page"
            text " to reconnect."

        div ! id "scores-container" $ text ""

type PlayerReadyInfo
  = { ready :: Boolean
    , name  :: String
    }

waitingMessage :: forall void.
  ClientStateWaiting -> PlayerId -> M.Map PlayerId String -> Html void
waitingMessage sw pId playersMap = do
  whenJust sw.prevGame (scoresTable pId playersMap)

  let r = fromMaybe NotReady $ M.lookup pId sw.readyStates
  p $ text $ case r of
               Ready -> "Waiting for other players..."
               NotReady -> "Press SPACE when you're ready"

  div ! className "clearfix" $
    for_ allPlayerIds $ \pId' -> do
      let mInfo = getPlayerInfo pId'
      let cl = "ready-state" <>
                    (if pId' == pId then " is-you" else "") <>
                    (if isJust mInfo then "" else " not-connected")

      div ! className cl $ do
        let pId'str = displayPlayerId pId'
        let cl' = "player-" <> pId'str
        p ! className cl' $ text pId'str
        whenJust mInfo $ \info -> do
          p $ text info.name
          p $ text $ case info.ready of
                       Ready -> "ready!"
                       NotReady -> "not ready"
  where
  getPlayerInfo pId'' = do
    ready <- M.lookup pId'' sw.readyStates
    name <- M.lookup pId'' playersMap
    pure $ { ready: ready, name: name }

type PlayerScoreInfo
  = { score :: Int
    , name  :: String
    , pId   :: PlayerId
    }

scoresTable :: forall void. PlayerId -> M.Map PlayerId String -> Game -> Html void
scoresTable pId playersMap game =
  div ! className "scores clearfix" $ do
    h2 ! className "scores-header" $ text "scores"
    div ! className "scores-table" $ do
      for_ (sortedPlayerInfos game) $ \info -> do
        let cl = "scores-row" <> (if info.pId == pId then " is-you" else "")
        div ! className cl $ do
          let pIdStr = displayPlayerId info.pId
          scoresCellDiv ["cell-thin", "player-" <> pIdStr] pIdStr
          scoresCellDiv ["cell-wide"] info.name
          scoresCellDiv ["cell-thin", "score"] (show info.score)
  where
  sortedPlayerInfos game =
    List.reverse $ List.sortBy (compare `on` score) (playerInfos game playersMap)

  score i = i.score

  scoresCellDiv classes innerText =
    div ! className (String.joinWith " " $ ["scores-cell"] <> classes) $
      text innerText


playerInfos game playersMap =
  List.catMaybes $ map (getPlayerInfo playersMap) allPlayers
  where
  allPlayers = M.toUnfoldable (game ^. players)


getPlayerInfo ::
  M.Map PlayerId String -> Tuple PlayerId Player -> Maybe PlayerScoreInfo
getPlayerInfo playersMap (Tuple pId'' p) = do
  name <- M.lookup pId'' playersMap
  pure $ { score: p ^. pScore, name: name, pId: pId'' }

simpleScores :: forall e. M.Map PlayerId String -> Game -> Html e
simpleScores playersMap game = do
  div ! className "simple-scores" $ do
    for_ (playerInfos game playersMap) $ \info -> do
      let pId = displayPlayerId info.pId
      scoresCellDiv ["cell-thinnest", "player-" <> pId] pId
      scoresCellDiv ["cell-thinnest", "score"] (show info.score)

  where
  scoresCellDiv classes innerText =
    div ! className (String.joinWith " " $ ["scores-cell"] <> classes) $
      text innerText
