{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Backup
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
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
import            Data.Time.Clock
import            Data.Time.LocalTime
import            Data.Time.Format


backupLabel :: String -> String -> String
backupLabel name time = name ++ "-backup-" ++ time ++ ".tar.gz"


fullBackup :: TmuxID -> FilePath -> FilePath -> [FilePath] -> IO ()
fullBackup t rootDir backupDir targets = do
    sendTmux t "say SERVER BACKUP STARTING. Server going readonly..."
    time <- getCurrentTime
    zone <- getCurrentTimeZone
    let label = backupLabel "ecserver3" $ formatTime defaultTimeLocale "%y-%m-%dT%H-%M-%S" (utcToLocalTime zone time)
    sendTmux t "save-off"
    sendTmux t "save-all"
    sendTmux t "say Archiving..."
    tarball <- Tar.pack rootDir targets
    sendTmux t "save-on"
    sendTmux t "say Compressing..."
    BL.writeFile (backupDir ++ "/" ++ label) . GZip.compress . Tar.write $ tarball
    sendTmux t "say Backup Complete!"
