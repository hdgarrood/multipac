module Server where

import Debug.Trace
import Data.Foreign.EasyFFI
import Data.Function
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Node.Express.App
import Node.Express.Handler

app :: App
app = do
    get "/" (sendFile "html/index.html")
    get "/js/client.js" (sendFile "js/client.js")

main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listen app port \_ ->
        trace $ "Listening on " ++ show port
