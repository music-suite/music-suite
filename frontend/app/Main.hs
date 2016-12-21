
module Main where

-- import Lib
import Data.String
import Lubeck.FRP
import Lubeck.App(runAppReactive)
import Web.VirtualDom.Html(text)

import Data.Aeson
import JavaScript.Web.WebSocket

main :: IO ()
main = do
  -- let n = someFunc
  -- runAppReactive $ pure $ text $ fromString $ "The number is " ++ show n

-- TODO Share

data MessageToServer =
  PingToServer Int
  ResponseToPingToClient Int
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

data MessageToClient =
  PingToClient Int
  ResponseToPingToServer Int
  deriving (Eq, Ord, Show, ToJSON, FromJSON)


createBackendConn messageToServerE = do
  (closeS, closeE) <- newEvent
  (messageS, messageFromServerE) <- newEvent
  socket <- connect $ WebSocketRequest
            { url = "websocket://localhost:68742"
            , protocols = []
            , onClose = Just (\(CloseEvent x) -> closeS x)
            , onMessage = Just (\(MessageEvent x) -> messageS x)
            }
  subscribeEvent messageToServerE $ \msg -> send msg socket
