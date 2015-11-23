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
import            OpenSandbox.Minecraft.Update
import            OpenSandbox.Tmux
import            Options.Applicative
import            System.Directory
import            System.IO
import            System.Process

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
           ]


-- | Takes the absolute path to the server's directory and the server's version
-- and constructs the shell command to run in Tmux.
minecraftServiceCmd :: FilePath -> String -> String
minecraftServiceCmd rootPath v = "cd " ++ rootPath ++"; java -Xmx4G -Xms512M -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalPacing -XX:ParallelGCThreads=4 -XX:+AggressiveOpts -jar minecraft_server." ++ v ++ ".jar nogui"


runMinecraftServer :: [String] -> Service -> IO ()
runMinecraftServer args srv = callCommand ("cd " ++ srvPath srv ++"; java -jar " ++ srvPath srv ++ "/" ++ mcServerJar (srvVersion srv) ++ " nogui")


setupNewServer :: String -> IO ()
setupNewServer n = do
    putStrLn "Welcome to Open Sandbox's Interactive Server Setup!"
    putStrLn "Please provide the following:"
    putStr "Port: "
    hFlush stdout
    p <- getLine
    putStr "Server Path (eg. /srv/minecraft)?: "
    hFlush stdout
    r <- getLine
    putStr "Version: "
    hFlush stdout
    v <- getLine
    putStrLn "Setting up new server..."
    let newServer = Service
                      n
                      ("opensandbox:"++p)
                      r
                      (r ++ "/" ++ "backup")
                      (r ++ "/" ++ "logs")
                      "world"
                      v
    createDirectoryIfMissing True (srvPath newServer)
    createDirectoryIfMissing True (srvBackupPath newServer)
    putStr $ "Downloading minecraft." ++ v ++ ".jar..."
    getMCSnapshot (srvPath newServer) (srvVersion newServer)
    putStrLn "[Done]"
    writeFile (srvPath newServer ++ "/server.properties") ("server-port="++p)
    runMinecraftServer [] newServer
    putStrLn "----------------------------------------------------------------"
    putStrLn "|           << Mojang's End User License Agreement >>          |"
    putStrLn "----------------------------------------------------------------"
    eula <- readFile $ srvPath newServer ++ "/" ++ "eula.txt"
    mapM_ putStrLn (lines eula)
    putStrLn "Do you Agree? (y/n)"
    c <- getChar
    when (c == 'y') $
      writeFile (srvPath newServer ++ "/" ++ "eula.txt")
                (unlines (init (lines eula) ++ ["eula=true"]))
    newWindow (srvTmuxID newServer) (srvPath newServer) n
    sendTmux (srvTmuxID newServer) (minecraftServiceCmd (srvPath newServer) (srvVersion newServer))
    callCommand "sleep 10"
    killWindow p
    putStrLn "Server Setup Complete!"


boot :: IO ()
boot = tmuxInit


shutdown :: IO ()
shutdown = tmuxClose


create :: Services -> ServiceName -> IO ()
create slst n =
    case Map.lookup n slst of
      Just s  -> putStrLn "Error: Service already exists!"
      Nothing -> setupNewServer n


start :: Services -> ServiceName -> IO ()
start slst n =
    case Map.lookup n slst of
      Just s    -> mkTmuxWindow s >> launchServerInWindow s
      Nothing   -> putStrLn $ "Error: Cannot find service " ++ n ++ "!"
  where mkTmuxWindow s = newWindow (srvTmuxID s) (srvPath s) n
        launchServerInWindow s = sendTmux (srvTmuxID s) (minecraftServiceCmd (srvPath s) (srvVersion s))


stop :: Services -> ServiceName -> IO ()
stop slst n =
    case Map.lookup n slst of
      Just s  -> sendTmux (srvTmuxID s) "stop" >> callCommand "sleep 5" >> killWindow (srvTmuxID s)
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
backup slst n =
    case Map.lookup n slst of
      Just s -> fullBackup  (srvTmuxID s)
                            (srvPath s)
                            (srvBackupPath s)
                            [srvWorld s, "minecraft_server." ++ srvVersion s ++ ".jar"]
      Nothing -> putStrLn $ "Error: Cannot find service " ++ n ++ "!"


upgrade :: Services -> ServiceName -> String -> String -> IO ()
upgrade slst n "to" v = putStrLn $ "Upgrading " ++ n ++ "..."
upgrade slst n _ v = putStrLn "Error: Invalid command syntax!"


downgrade :: Services -> ServiceName -> String -> String -> IO ()
downgrade slst n "to" v = putStrLn $ "Downgrading " ++ n ++ "..."
downgrade slst n _ v = putStrLn "Error: Invalid command syntax!"


-- | Executes the 'say' command in the target Minecraft server.
-- Must be provided the list of services, the target service,
-- and the message to say on the server.
say :: Services -> ServiceName -> String -> IO ()
say slst s m =
  case Map.lookup s slst of
      Just service  -> sendTmux (srvTmuxID service) ("say " ++ m)
      Nothing       -> putStrLn "Error: Service cannot be found!"


with :: Services -> ServiceName -> String -> IO ()
with slst s c = putStrLn $ "Running command " ++ c ++ "..."


commands :: Services -> Parser (IO ())
commands slst = subparser
    (  command "boot"
      (info (helper <*> pure boot)
        (fullDesc
        <> progDesc "Boots the Tmux Server"
        <> header "boot - starts the Tmux Server"))
    <> command "shutdown"
      (info (helper <*> pure shutdown)
        (fullDesc
        <> progDesc "Shuts down the Tmux Server"
        <> header "shutdown - closes the Tmux Server"))
    <> command "create"
      (info (helper <*> (create slst <$> argument str idm))
        (fullDesc
        <> progDesc "minecraftctl create TARGET"
        <> header "create - creates a new minecraft server"))
    <> command "start"
      (info (helper <*> (start slst <$> argument str idm))
        (fullDesc
        <> progDesc "Starts a TARGET server"
        <> header "start - starts a server"))
    <> command "stop"
      (info (helper <*> (stop slst <$> argument str idm))
        (fullDesc
        <> progDesc "Stops a TARGET server"
        <> header "stop - stops a server"))
    <> command "restart"
      (info (helper <*> (restart slst <$> argument str idm))
        (fullDesc
        <> progDesc "Restarts a TARGET server"
        <> header "restart - restarts a server"))
    <> command "status"
      (info (helper <*> (status slst <$> argument str idm))
        (fullDesc
        <> progDesc "Obtains the status of TARGET service"
        <> header "status - get the status of the service"))
    <> command "enable"
      (info (helper <*> (enable slst <$> argument str idm))
        (fullDesc
        <> progDesc "Enables a TARGET server"
        <> header "enable - enables a server"))
    <> command "disable"
      (info (helper <*> (disable slst <$> argument str idm))
        (fullDesc
        <> progDesc "Disables a TARGET server"
        <> header "disable - disables a server"))
    <> command "reload"
      (info (helper <*> (reload slst <$> argument str idm))
        (fullDesc
        <> progDesc "Reloads a TARGET server"
        <> header "reload - reload a server"))
    <> command "whoison"
      (info (helper <*> (whoison slst <$> argument str idm))
        (fullDesc
        <> progDesc "Lists all users currently logged in on TARGET"
        <> header "whoison - lists logged in users"))
    <> command "backup"
      (info (helper <*> (backup slst <$> argument str idm))
        (fullDesc
        <> progDesc "Backs up a TARGET server"
        <> header "backup - back ups a server"))
    <> command "upgrade"
      (info (helper <*> (upgrade slst <$> argument str idm <*> argument str idm <*> argument str idm))
        (fullDesc
        <> progDesc "Updates a TARGET server"
        <> header "upgrade - upgrades a server to the given version"))
    <> command "downgrade"
      (info (helper <*> (downgrade slst <$> argument str idm <*> argument str idm <*> argument str idm))
        (fullDesc
        <> progDesc "Downgrades a TARGET server"
        <> header "downgrade - downgrades a server to the given version"))
    <> command "say"
      (info (helper <*> (say slst <$> argument str idm <*> argument str idm))
        (fullDesc
        <> progDesc "minecraftctl say TARGET"
        <> header "say - 'say' something on a server"))
    <> command "with"
      (info (helper <*> (with slst <$> argument str idm <*> argument str idm))
        (fullDesc
        <> progDesc "with a TARGET server, run the following COMMAND"
        <> header "with - Run server commands via a given server")))

opts slst = info (helper <*> commands slst)
    ( fullDesc
   <> progDesc "Controls Minecraft Servers"
   <> header "minecraftctl - [OPTIONS...] {COMMAND} ...")


main :: IO ()
main = do
    --hSetBuffering stdin NoBuffering
    let defaultServices = Map.singleton "ecServer" ecServer
    join $ execParser (opts defaultServices)
