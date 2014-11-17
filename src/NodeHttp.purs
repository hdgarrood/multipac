module NodeHttp where

import Data.Array (length, (!!))
import Data.Maybe
import Data.String (split)
import Data.Function
import Control.Monad.Eff

foreign import data Http :: !
foreign import data Server :: *
foreign import data Request :: *
foreign import data Response :: *

foreign import http "var http = require('http')" :: Unit
foreign import url "var url = require('url')" :: Unit
foreign import fs "var fs = require('fs')" :: Unit

type Port = Number
type Url = { href :: String, search :: String, query :: String, pathname :: String }

foreign import createServer
  """
  function createServer(action) {
    return function() {
      return http.createServer(function(req, res) {
        action(req)(res)()
      })
    }
  }""" :: forall e a.
  (Request -> Response -> Eff (http :: Http | e) a)
  -> Eff (http :: Http | e) Server

foreign import listenImpl
  """
  function listenImpl(server, port) {
    return function() {
      server.listen(port)
    }
  }
  """ :: forall e. Fn2 Server Port (Eff (http :: Http | e) Unit)

listen :: forall e.
  Server -> Port -> Eff (http :: Http | e) Unit
listen server port = runFn2 listenImpl server port

foreign import getUrl
  """
  function getUrl(req) {
    return url.parse(req.url)
  }
  """ :: Request -> Url

foreign import sendFileImpl
  """
  function sendFileImpl(path, res, detectMimeType) {
    return function() {
      fs.readFile(path, function(err, data) {
        if (err) { throw err }
        var mime = detectMimeType(path)
        res.writeHead(200, { 'Content-Type': mime || 'text/plain' })
        res.write(data)
        res.end()
      })
    }
  }
  """ :: forall e.
  Fn3 String Response (String -> String) (Eff (http :: Http | e) Unit)

sendFile :: forall e.
  String -> Response -> Eff (http :: Http | e) Unit
sendFile path res =
  runFn3 sendFileImpl path res (fromMaybe "text/plain" <<< detectMime)

-- detect the most likely mime type for a given filename
detectMime :: String -> Maybe String
detectMime str = do
  ext <- extension str
  case ext of
    "txt"  -> Just "text/plain"
    "html" -> Just "text/html"
    "css"  -> Just "text/css"
    "js"   -> Just "text/javascript"
    _      -> Nothing

extension :: String -> Maybe String
extension str =
  let arr = split "." str
      len = length arr
  in  arr !! (len - 1)

foreign import send404
  """
  function send404(res) {
    return function() {
      res.writeHead(404)
      res.write('404 not found')
      res.end()
    }
  }
  """ :: forall e. Response -> Eff (http :: Http | e) Unit
