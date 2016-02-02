{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Backup
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (requires Tmux)
--
-------------------------------------------------------------------------------
module OpenSandbox.Backup
    ( backupLabel
    , fullBackup
    ) where

import            OpenSandbox.Server
import            OpenSandbox.Tmux
import qualified  Codec.Archive.Tar as Tar
import qualified  Codec.Compression.GZip as GZip
import qualified  Data.ByteString.Lazy as BL
import            Data.Thyme.LocalTime (getZonedTime)
import            Data.Thyme.Format (formatTime)
import            System.Locale (defaultTimeLocale)

backupLabel :: String -> String -> String
backupLabel name time = name ++ "-backup-" ++ time ++ ".tar.gz"


fullBackup :: TmuxID -> FilePath -> FilePath -> [FilePath] -> IO ()
fullBackup t rootDir backupDir targets = do
    sendTmux t "say SERVER BACKUP STARTING. Server going readonly..."
    localTime <- getZonedTime
    let label = backupLabel "ecserver3" $ formatTime defaultTimeLocale "%y-%m-%dT%H-%M-%S" localTime
    sendTmux t "save-off"
    sendTmux t "save-all"
    sendTmux t "say Archiving..."
    tarball <- Tar.pack rootDir targets
    sendTmux t "save-on"
    sendTmux t "say Compressing..."
    BL.writeFile (backupDir ++ "/" ++ label) . GZip.compress . Tar.write $ tarball
    sendTmux t "say Backup Complete!"
