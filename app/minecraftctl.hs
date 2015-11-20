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

import            Control.Monad
import qualified  Data.Map as M
import            OpenSandbox
import            OpenSandbox.Tmux
import            Options.Applicative
import            Data.UUID

data Servers = Map String Server deriving (Show,Eq,Ord)

type ServerName = String

data Server = Server
  { srvUUID       :: !UUID
  , srvName       :: !ServerName
  , srvTmuxID     :: !TmuxID
  , srvDir        :: !FilePath
  , srvBackupDir  :: !FilePath
  } deriving (Show,Eq,Ord)


start :: TmuxID -> IO ()
start x = putStrLn $ "Starting " ++ x ++ "..."


stop :: String -> IO ()
stop x = putStrLn $ "Stopping " ++ x ++ "..."


enable :: String -> IO ()
enable x = putStrLn $ "Disabling " ++ x ++ "..."


disable :: String -> IO ()
disable x = putStrLn $ "Disabling " ++ x ++ "..."


restart :: String -> IO ()
restart x = putStrLn $ "Restarting " ++ x ++ "..."


reload :: String -> IO ()
reload t = putStrLn $ "Reloading " ++ t ++ "..."


who :: String -> IO ()
who x = putStrLn $ "The following users are logged into " ++ x ++ "..."


backup :: TmuxID -> String -> IO ()
backup t x = putStrLn $ "Backing up " ++ x ++ "..."


say :: TmuxID -> String -> IO ()
say t m = sendTmux t $ "say " ++ m


commands :: Parser (IO ())
commands = subparser
     ( command "start"
      (info (helper <*> (start <$> argument str idm))
        (fullDesc
        <> progDesc "Starts a TARGET server"
        <> header "start - starts a server"))
    <> command "stop"
      (info (helper <*> (stop <$> argument str idm))
        (fullDesc
        <> progDesc "Stops a TARGET server"
        <> header "stop - stops a server"))
    <> command "restart"
      (info (helper <*> (restart <$> argument str idm))
        (fullDesc
        <> progDesc "Restarts a TARGET server"
        <> header "restart - restarts a server"))
    <> command "enable"
      (info (helper <*> (enable <$> argument str idm))
        (fullDesc
        <> progDesc "Enables a TARGET server"
        <> header "enable - enables a server"))
    <> command "disable"
      (info (helper <*> (disable <$> argument str idm))
        (fullDesc
        <> progDesc "Disables a TARGET server"
        <> header "disable - disables a server"))
    <> command "reload"
      (info (helper <*> (reload <$> argument str idm))
        (fullDesc
        <> progDesc "Reloads a TARGET server"
        <> header "reload - reload a server"))
    <> command "who"
      (info (helper <*> (who <$> argument str idm))
        (fullDesc
        <> progDesc "Lists all users currently logged in on TARGET"
        <> header "who - lists logged in users"))
    <> command "backup"
      (info (helper <*> (backup <$> argument str idm <*> argument str idm))
        (fullDesc
        <> progDesc "Backs up a TARGET server"
        <> header "backup - back ups a server"))
    <> command "say"
      (info (helper <*> (say <$> argument str idm <*> argument str idm))
        (   fullDesc
        <>  progDesc "Sends a 'say' command to a TARGET server"
        <>  header "say - 'say' something on a server")))


opts = info (helper <*> commands)
    ( fullDesc
   <> progDesc "Controls Minecraft Servers"
   <> header "minecraftctl - A tool for controlling Minecraft servers")


main :: IO ()
main = join $ execParser opts
