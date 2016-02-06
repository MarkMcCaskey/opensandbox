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
import            OpenSandbox
import            System.Directory


main :: IO ()
main = do
    args <- getOpts opensandboxOpts
    let config = debugConfig
    -- Start Logger
    createDirectoryIfMissing True (srvPath config ++ "/" ++ srvLogPath config)
    let logFilePath =  srvPath config ++ "/" ++ srvLogPath config ++ "/" ++ "latest.log"
    logger <- newLogger defaultBufSize logFilePath (fromJust args)
    writeTo logger Info "----------------- Log Start -----------------"
    writeTo logger Info "Welcome to the OpenSandbox Minecraft Server!"
    writeTo logger Info $ "Starting minecraft server version " ++ show (srvMCVersion config)
    writeTo logger Info $ "Starting Minecraft server on " ++ show (srvPort config)
    writeTo logger Info $ "Done!"
    runOpenSandboxServer config logger
