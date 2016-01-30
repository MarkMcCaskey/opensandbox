{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- File         : minecraftctl.hs
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (requires Tmux)
--
-------------------------------------------------------------------------------

import            Control.Concurrent
import            Control.Monad
import qualified  Data.Map as Map
import            Data.String
import            Data.UUID hiding (fromString)
import            OpenSandbox
import            OpenSandbox.Backup
import            OpenSandbox.Update
import            OpenSandbox.Server
import            OpenSandbox.Tmux
import            Options.Applicative
import            System.Directory
import            System.IO
import            System.Process


testServer :: Server
testServer = Server
  { srvName = "test"
  , srvPort = 25566
  , srvPath = "/srv/test"
  , srvBackupPath = "/srv/test/backup"
  , srvLogPath = "/srv/test/logs"
  , srvWorld = "world"
  , srvVersion = "16w04a"
  , srvPlayers = 0
  , srvMaxPlayers = 20
  , srvMotd = "A Minecraft server"
  , srvEncryption = Nothing
  , srvCompression = Nothing
  , srvEnabled = True
  , srvUp = False
  }


ecServer :: Server
ecServer = Server
  { srvName = "ecServer"
  , srvPort = 25565
  , srvPath = "/srv/minecraft"
  , srvBackupPath = "/home/oldmanmike/backup"
  , srvLogPath = "/srv/minecraft/logs"
  , srvWorld = "world"
  , srvVersion = "16w04a"
  , srvPlayers = 0
  , srvMaxPlayers = 20
  , srvMotd = "A Minecraft server"
  , srvEncryption = Nothing
  , srvCompression = Nothing
  , srvEnabled = True
  , srvUp = False
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


runMinecraftServer :: [String] -> Server -> IO ()
runMinecraftServer args srv = callCommand ("cd " ++ srvPath srv ++"; java -jar " ++ srvPath srv ++ "/" ++ mcServerJar (srvVersion srv) ++ " nogui")


prompt :: String -> Maybe String -> IO String
prompt prompt maybeInput =
    case maybeInput of
      Just input -> return $ show input
      Nothing -> do putStr prompt
                    hFlush stdout
                    x <- getLine
                    return x


promptInt :: String -> Maybe Int -> IO Int
promptInt prompt maybeInput =
    case maybeInput of
      Just input -> return input
      Nothing -> do putStr prompt
                    hFlush stdout
                    x <- getLine
                    return (read x :: Int)


setupNewServer :: String -> Maybe Int -> Maybe FilePath -> Maybe String -> Maybe String -> IO ()
setupNewServer n maybeP maybeR maybeW maybeV = do
    putStrLn "Welcome to Open Sandbox's Interactive Server Setup!"
    putStrLn "Please provide the following:"
    p <- promptInt "Port: " maybeP
    r <- prompt "Server Path (eg. /srv/minecraft): " maybeR
    w <- prompt "World Name: " maybeW
    v <- prompt "Version: " maybeV
    putStrLn "Setting up new server..."
    let newServer = Server n (toEnum p) r (r++"/backup") (r++"/logs") w v 0 20 "" Nothing Nothing False False
    createDirectoryIfMissing True (srvPath newServer)
    createDirectoryIfMissing True (srvBackupPath newServer)
    putStr $ "Downloading minecraft." ++ v ++ ".jar..."
    hFlush stdout
    getMCSnapshot (srvPath newServer) (srvVersion newServer)
    putStrLn "[Done]"
    writeFile (srvPath newServer ++ "/server.properties") ("server-port="++show p++"\n"++"level-name="++w)
    runMinecraftServer [] newServer
    putStrLn "----------------------------------------------------------------"
    putStrLn "|           << Mojang's End User License Agreement >>          |"
    putStrLn "----------------------------------------------------------------"
    eula <- readFile $ srvPath newServer ++ "/eula.txt"
    mapM_ putStrLn (lines eula)
    putStrLn "Do you Agree? (y/n)"
    c <- getChar
    when (c == 'y') $
      writeFile (srvPath newServer ++ "/eula.txt")
                (unlines (init (lines eula) ++ ["eula=true"]))
    newWindow (tmuxID newServer) (srvPath newServer) n
    sendTmux (tmuxID newServer) (minecraftServiceCmd (srvPath newServer) (srvVersion newServer))
    threadDelay 10000000
    killWindow $ show p
    putStrLn "Server Setup Complete!"


boot :: IO ()
boot = tmuxInit


shutdown :: IO ()
shutdown = tmuxClose


create :: Servers -> String -> Maybe Int -> Maybe String -> Maybe String -> Maybe String -> IO ()
create slst n p r w v =
    case Map.lookup n slst of
      Just s  -> putStrLn "Error: Service already exists!"
      Nothing -> setupNewServer n p r w v


start :: Servers -> String -> IO ()
start slst n =
    case Map.lookup n slst of
      Just s    -> mkTmuxWindow s >> launchServerInWindow s
      Nothing   -> putStrLn $ "Error: Cannot find service " ++ n ++ "!"
  where mkTmuxWindow s = newWindow (tmuxID s) (srvPath s) n
        launchServerInWindow s = sendTmux (tmuxID s) (minecraftServiceCmd (srvPath s) (srvVersion s))


stop :: Servers -> String -> IO ()
stop slst n =
    case Map.lookup n slst of
      Just s  -> sendTmux (tmuxID s) "stop" >> callCommand "sleep 5" >> killWindow (show $ srvPort s)
      Nothing -> putStrLn $ "Error: Cannot find service " ++ n ++ "!"


status :: Servers -> String -> IO ()
status slst n = putStrLn $ "Getting status of " ++ n ++ "..."


enable :: Servers -> String -> IO ()
enable slst n = putStrLn $ "Disabling " ++ n ++ "..."


disable :: Servers -> String -> IO ()
disable slst n = putStrLn $ "Disabling " ++ n ++ "..."


restart :: Servers -> String -> IO ()
restart slst n = putStrLn $ "Restarting " ++ n ++ "..."


reload :: Servers -> String -> IO ()
reload slst n = putStrLn $ "Reloading " ++ n ++ "..."


whoison :: Servers -> String -> IO ()
whoison slst n = putStrLn $ "The following users are logged into " ++ n ++ "..."


-- | Backs up the target Minecraft service.
backup :: Servers -> String -> IO ()
backup slst n =
    case Map.lookup n slst of
      Just s -> fullBackup  (tmuxID s)
                            (srvPath s)
                            (srvBackupPath s)
                            [srvWorld s, "minecraft_server." ++ srvVersion s ++ ".jar"]
      Nothing -> putStrLn $ "Error: Cannot find service " ++ n ++ "!"


upgrade :: Servers -> String -> String -> String -> IO ()
upgrade slst n "to" v = putStrLn $ "Upgrading " ++ n ++ "..."
upgrade slst n _ v = putStrLn "Error: Invalid command syntax!"


downgrade :: Servers -> String -> String -> String -> IO ()
downgrade slst n "to" v = putStrLn $ "Downgrading " ++ n ++ "..."
downgrade slst n _ v = putStrLn "Error: Invalid command syntax!"


-- | Executes the 'say' command in the target Minecraft server.
-- Must be provided the list of services, the target service,
-- and the message to say on the server.
say :: Servers -> String -> String -> IO ()
say slst s m =
  case Map.lookup s slst of
      Just s  -> sendTmux (tmuxID s) ("say " ++ m)
      Nothing -> putStrLn "Error: Service cannot be found!"


with :: Servers -> String -> String -> IO ()
with slst s c = putStrLn $ "Running command " ++ c ++ "..."


portOption :: Parser (Maybe Int)
portOption = optional $ option auto
  ( long "port"
  <> short 'p'
  <> metavar "LABEL"
  <> help "Assign a service a PORT")


rootPathOption :: Parser (Maybe String)
rootPathOption = optional $ strOption
  ( long "rootpath"
  <> short 'r'
  <> metavar "ROOTPATH"
  <> help "Assign a service a root location ROOTPATH")


worldOption :: Parser (Maybe String)
worldOption = optional $ strOption
  ( long "world"
  <> short 'w'
  <> metavar "WORLD"
  <> help "Assign a service a WORLD")


versionOption :: Parser (Maybe String)
versionOption = optional $ strOption
  ( long "version"
  <> short 'v'
  <> metavar "WORLD"
  <> help "Assign a service a game VERSION")


commands :: Servers -> Parser (IO ())
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
      (info (helper <*> (create slst
                      <$> argument str idm
                      <*> portOption
                      <*> rootPathOption
                      <*> worldOption
                      <*> versionOption))
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
    let defaultServices = Map.singleton "ecServer" ecServer
    join $ execParser (opts defaultServices)
