{-# LANGUAGE OverloadedStrings #-}
module OpenSandbox.Monitor
  ( monitor
  ) where

import            Control.Concurrent
import            Control.Exception (SomeException)
import            Control.Exception.Lifted (handle)
import            Control.Monad.IO.Class (liftIO)
import            Data.Aeson (Value, encode, object, (.=))
import            Data.Aeson.Parser (json)
import            Data.ByteString (ByteString)
import            Data.Conduit (($$))
import            Data.Conduit.Attoparsec (sinkParser)
import            GHC.Stats
import            Network.HTTP.Types (status200, status400)
import            Network.Wai (Application, Response, responseLBS)
import            Network.Wai.Conduit (sourceRequestBody)
import            Network.Wai.Handler.Warp (run)
import            System.Metrics
import            System.Metrics.Json

monitor :: Store -> Application
monitor store req sendResponse = handle (sendResponse . invalidJson) $ do
  --value <- sourceRequestBody req $$ sinkParser json
  sample <- liftIO $ fmap sampleToJson (sampleAll store)
  sendResponse $ responseLBS
    status200
    [("Content-Type", "application/json")]
    $ encode sample

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS
  status400
  [("Content-Type", "application/json")]
  $ encode $ object
      [ ("message" .= show ex)
      ]
