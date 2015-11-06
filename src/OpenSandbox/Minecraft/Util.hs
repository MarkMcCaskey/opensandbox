{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Util
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Util
    ( backupLabel
    , backup
    ) where


import qualified  Codec.Archive.Tar as Tar
import qualified  Codec.Compression.GZip as GZip
import qualified  Data.ByteString.Lazy as BL
import            Data.Time.Clock
import            Data.Time.Calendar
import            Turtle.Prelude


backupLabel :: String -> UTCTime -> String
backupLabel name time = name ++ "-backup-" ++ ((head.words.show) time)


backup :: FilePath -> FilePath -> [FilePath] -> IO ()
backup backupDir rootDir targets = do
    -- Set server to save-off
    -- Set server to save-all
    time <- getCurrentTime
    let label = backupLabel rootDir time
    tarball <- Tar.pack rootDir targets
    -- Set server to save-on
    BL.writeFile (backupDir ++ "/" ++ label) . GZip.compress . Tar.write $ tarball

