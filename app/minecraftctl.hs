-------------------------------------------------------------------------------
-- |
-- File         : minecraftctl.hs
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------

import Control.Monad
import OpenSandbox
import Options.Applicative


start :: String -> IO ()
start x = putStrLn $ "Starting " ++ x ++ "..."


stop :: String -> IO ()
stop x = putStrLn $ "Stopping " ++ x ++ "..."


restart :: String -> IO ()
restart x = putStrLn $ "Restarting " ++ x ++ "..."


backup :: String -> IO ()
backup x = putStrLn $ "Backing up " ++ x ++ "..."


say :: String -> IO ()
say x = putStrLn x


opts :: Parser (IO ())
opts = subparser
     ( command "start" (info (start <$> argument str idm) idm)
    <> command "stop" (info (stop <$> argument str idm) idm)
    <> command "restart" (info (restart <$> argument str idm) idm)
    <> command "backup" (info (backup <$> argument str idm) idm)
    <> command "say" (info (say <$> argument str idm) idm))


main :: IO ()
main = join $ execParser (info opts idm)
