module NodeHttp where

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
    return url.parse
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
  """ :: forall e. Fn2 String Response (Eff (http :: Http | e) Unit)

sendFile :: forall e. String -> Response -> Eff (http :: Http | e) Unit
sendFile path res = runFn2 sendFileImpl path res

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
