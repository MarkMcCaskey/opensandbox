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
import qualified  Data.Map as Map
import            Data.UUID
import            OpenSandbox
import            OpenSandbox.Minecraft.Backup
import            OpenSandbox.Tmux
import            Options.Applicative
import            Prelude hiding (init)


type Services = Map.Map ServiceName Service

type ServiceName = String

data Service = Service
  { srvName       :: !ServiceName
  , srvTmuxID     :: !TmuxID
  , srvPath       :: !FilePath
  , srvBackupPath :: !FilePath
  , srvLogPath    :: !FilePath
  , srvWorld      :: !String
  , srvVersion    :: !String
  } deriving (Show,Eq,Ord)

testServer :: Service
testServer = Service
  { srvName = "test"
  , srvTmuxID = "opensandbox:25566"
  , srvPath = "/srv/test"
  , srvBackupPath = "/srv/test/backup"
  , srvLogPath = "/srv/test/logs"
  , srvWorld = "world"
  , srvVersion = "15w47c"
  }

ecServer :: Service
ecServer = Service
  { srvName = "ecServer"
  , srvTmuxID = "opensandbox:25565"
  , srvPath = "/srv/minecraft"
  , srvBackupPath = "/home/oldmanmike/backup"
  , srvLogPath = "/srv/minecraft/logs"
  , srvWorld = "world"
  , srvVersion = "15w47c"
  }


javaArgs :: [String]
javaArgs = [ "-Xmx4G"
           , "-Xms512M"
           , "-XX:+UseConcMarkSweepGC"
           , "-XX:+CMSIncrementalPacing"
           , "-XX:ParallelGCThreads=4"
           , "-XX:+AggressiveOpts"
           , "nogui"
           ]


minecraftServiceCmd :: String
minecraftServiceCmd = "su minecraft -s /bin/bash -c \"cd /srv/minecraft; java -Xmx4G -Xms512M -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalPacing -XX:ParallelGCThreads=4 -XX:+AggressiveOpts -jar /srv/minecraft/minecraft_server.15w47c.jar nogui\""


init :: IO ()
init = tmuxInit

close :: IO ()
close = tmuxClose

start :: Services -> ServiceName -> IO ()
start slst s = do
    putStrLn $ "Starting " ++ s ++ "..."
    case (Map.lookup s slst) of
      Just service  -> newWindow (srvTmuxID service) (srvPath service) s
      Nothing       -> putStrLn $ "Error: Cannot find service " ++ s ++ "!"


stop :: Services -> ServiceName -> IO ()
stop slst n = case (Map.lookup n slst) of
                Just s  -> sendTmux (srvTmuxID s) "stop"
                Nothing -> putStrLn $ "Error: Cannot find service " ++ n ++ "!"


status :: Services -> ServiceName -> IO ()
status slst n = putStrLn $ "Getting status of " ++ n ++ "..."


enable :: Services -> ServiceName -> IO ()
enable slst n = putStrLn $ "Disabling " ++ n ++ "..."


disable :: Services -> ServiceName -> IO ()
disable slst n = putStrLn $ "Disabling " ++ n ++ "..."


restart :: Services -> ServiceName -> IO ()
restart slst n = putStrLn $ "Restarting " ++ n ++ "..."


reload :: Services -> ServiceName -> IO ()
reload slst n = putStrLn $ "Reloading " ++ n ++ "..."


whoison :: Services -> ServiceName -> IO ()
whoison slst n = putStrLn $ "The following users are logged into " ++ n ++ "..."


-- | Backs up the target Minecraft service.
backup :: Services -> ServiceName -> IO ()
backup slst n = case (Map.lookup n slst) of
                  Just s -> fullBackup  (srvTmuxID s)
                                        (srvPath s)
                                        (srvBackupPath s)
                                        [(srvWorld s), ("minecraft_server." ++ (srvVersion s) ++ ".jar")]
                  Nothing -> putStrLn $ "Error: Cannot find service " ++ n ++ "!"


-- | Executes the 'say' command in the target Minecraft server.
-- Must be provided the list of services, the target service,
-- and the message to say on the server.
say :: Services -> ServiceName -> String -> IO ()
say slst s m = do
    case (Map.lookup s slst) of
      Just service  -> sendTmux (srvTmuxID service) ("say " ++ m)
      Nothing       -> putStrLn "Error: Service cannot be found!"

{-
with :: Services -> ServiceName -> String -> IO ()
with slst s c = do
    case (Map.lookup s slst) of
      Just service  -> sendTmux (srvTmuxID service) (c
-}

commands :: Services -> Parser (IO ())
commands slst = subparser
    (  command "init"
      (info (helper <*> (pure init))
        (fullDesc
        <> progDesc "Init the Tmux Server"
        <> header "init - starts the Tmux Server"))
    <> command "close"
      (info (helper <*> (pure close))
        (fullDesc
        <> progDesc "Closes the Tmux Server"
        <> header "close - closes the Tmux Server"))
    <> command "start"
      (info (helper <*> ((start slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Starts a TARGET server"
        <> header "start - starts a server"))
    <> command "stop"
      (info (helper <*> ((stop slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Stops a TARGET server"
        <> header "stop - stops a server"))
    <> command "restart"
      (info (helper <*> ((restart slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Restarts a TARGET server"
        <> header "restart - restarts a server"))
    <> command "status"
      (info (helper <*> ((status slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Obtains the status of TARGET service"
        <> header "status - get the status of the service"))
    <> command "enable"
      (info (helper <*> ((enable slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Enables a TARGET server"
        <> header "enable - enables a server"))
    <> command "disable"
      (info (helper <*> ((disable slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Disables a TARGET server"
        <> header "disable - disables a server"))
    <> command "reload"
      (info (helper <*> ((reload slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Reloads a TARGET server"
        <> header "reload - reload a server"))
    <> command "whoison"
      (info (helper <*> ((whoison slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Lists all users currently logged in on TARGET"
        <> header "whoison - lists logged in users"))
    <> command "backup"
      (info (helper <*> ((backup slst) <$> argument str idm))
        (fullDesc
        <> progDesc "Backs up a TARGET server"
        <> header "backup - back ups a server"))
    <> command "say"
      (info (helper <*> ((say slst) <$> argument str idm <*> argument str idm))
        (   fullDesc
        <>  progDesc "Sends a 'say' command to a TARGET server"
        <>  header "say - 'say' something on a server")))


opts slst = info (helper <*> (commands slst))
    ( fullDesc
   <> progDesc "Controls Minecraft Servers"
   <> header "minecraftctl - A tool for controlling Minecraft servers")


main :: IO ()
main = do
    let defaultServices = Map.singleton "ecServer" ecServer
    join $ execParser (opts defaultServices)
