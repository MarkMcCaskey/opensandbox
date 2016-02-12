{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- File         : server.hs
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------

import            Data.Maybe
import qualified  Data.Text as T
import            OpenSandbox
import            System.Directory
import            System.Exit

main :: IO ()
main = do
    args <- getOpts
    if getVersionFlag args
      then do print $ "OpenSandbox Version:   " `T.append` openSandboxVersion
              print $ "MC Stable Version:     " `T.append` majorVersion
              print $ "MC Snapshot Version:   " `T.append` snapshotVersion
              exitSuccess
      else return ()
    createDirectoryIfMissing True "config"
    maybeConfig <- loadConfig "./config/opensandboxd.yaml"
    let config = fromJust maybeConfig
    -- Start Logger
    createDirectoryIfMissing True (srvPath config ++ "/" ++ srvLogPath config)
    let logFilePath =  srvPath config ++ "/" ++ srvLogPath config ++ "/" ++ "latest.log"
    logger <- newLogger defaultBufSize logFilePath $
      if getDebugFlag args
        then Debug
        else Info
    writeTo logger Info "----------------- Log Start -----------------"
    writeTo logger Info "Welcome to the OpenSandbox Minecraft Server!"
    writeTo logger Info $ "Starting minecraft server version " ++ show snapshotVersion
    writeTo logger Info $ "Starting Minecraft server on " ++ show (srvPort config)
    writeTo logger Info $ "Done!"
    runOpenSandboxServer config logger
