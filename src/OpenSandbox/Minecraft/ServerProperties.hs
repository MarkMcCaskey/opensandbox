{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.ServerProperties
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.ServerProperties
    ( ServerProperties
    , readServerProperties
    ) where


import            Control.Error.Safe
import            Data.Char
import            Data.IP (IPv4 (..))
import qualified  Data.Text as T
import            Text.ParserCombinators.Parsec


newtype AllowFlight = AllowFlight Bool
    deriving (Eq,Ord,Read,Show)
newtype AllowNether = AllowNether Bool
    deriving (Eq,Ord,Read,Show)
newtype AnnouncePlayerAchievements = AnnouncePlayerAchievements Bool
    deriving (Eq,Ord,Read,Show)
newtype Difficulty = Difficulty Int
    deriving (Eq,Ord,Read,Show)
newtype EnableCommandBlock = EnableCommandBlock Bool
    deriving (Eq,Ord,Read,Show)
newtype EnableQuery = EnableQuery Bool
    deriving (Eq,Ord,Read,Show)
newtype EnableRcon = EnableRcon Bool
    deriving (Eq,Ord,Read,Show)
newtype ForceGameMode = ForceGameMode Bool
    deriving (Eq,Ord,Read,Show)
newtype GameMode = GameMode Int
    deriving (Eq,Ord,Read,Show)
newtype GenerateStructures = GenerateStructures Bool
    deriving (Eq,Ord,Read,Show)
newtype GeneratorSettings = GeneratorSettings T.Text
    deriving (Eq,Ord,Read,Show)
newtype Hardcore = Hardcore Bool
    deriving (Eq,Ord,Read,Show)
newtype LevelName = LevelName T.Text
    deriving (Eq,Ord,Read,Show)
newtype LevelSeed = LevelSeed Int
    deriving (Eq,Ord,Read,Show)
newtype LevelType = LevelType T.Text
    deriving (Eq,Ord,Read,Show)
newtype MaxBuildHeight = MaxBuildHeight Int
    deriving (Eq,Ord,Read,Show)
newtype MaxPlayers = MaxPlayers Int
    deriving (Eq,Ord,Read,Show)
newtype MaxTickTime = MaxTickTime Int
    deriving (Eq,Ord,Read,Show)
newtype MaxWorldSize = MaxWorldSize Int
    deriving (Eq,Ord,Read,Show)
newtype MOTD = MOTD T.Text
    deriving (Eq,Ord,Read,Show)
newtype NetworkCompressionThreshold = NetworkCompressionThreshold Int
    deriving (Eq,Ord,Read,Show)
newtype OnlineMode = OnlineMode Bool
    deriving (Eq,Ord,Read,Show)
newtype OpPermissionLevel = OpPermissionLevel Int
    deriving (Eq,Ord,Read,Show)
newtype PlayerIdleTimeout = PlayerIdleTimeout Int
    deriving (Eq,Ord,Read,Show)
newtype PVP = PVP Bool
    deriving (Eq,Ord,Read,Show)
newtype QueryPort = QueryPort Int
    deriving (Eq,Ord,Read,Show)
newtype RconPassword = RconPassword T.Text
    deriving (Eq,Ord,Read,Show)
newtype RconPort = RconPort Int
    deriving (Eq,Ord,Read,Show)
newtype ResourcePack = ResourcePack T.Text
    deriving (Eq,Ord,Read,Show)
newtype ResourcePackHash = ResourcePackHash T.Text
    deriving (Eq,Ord,Read,Show)
newtype ServerIP = ServerIP IPv4
    deriving (Eq,Ord,Read,Show)
newtype ServerPort = ServerPort Int
    deriving (Eq,Ord,Read,Show)
newtype SnooperEnabled = SnooperEnabled Bool
    deriving (Eq,Ord,Read,Show)
newtype SpawnAnimals = SpawnAnimals Bool
    deriving (Eq,Ord,Read,Show)
newtype SpawnMonsters = SpawnMonsters Bool
    deriving (Eq,Ord,Read,Show)
newtype SpawnNPCs = SpawnNPCs Bool
    deriving (Eq,Ord,Read,Show)
newtype SpawnProtection = SpawnProtection Int
    deriving (Eq,Ord,Read,Show)
newtype UseNativeTransport = UseNativeTransport Bool
    deriving (Eq,Ord,Read,Show)
newtype ViewDistance = ViewDistance Int
    deriving (Eq,Ord,Read,Show)
newtype WhiteList = WhiteList Bool
    deriving (Eq,Ord,Read,Show)

data ServerProperties = ServerProperties
    { allowFlight                   :: !AllowFlight
    , allowNether                   :: !AllowNether
    , announcePlayerAchievements    :: !AnnouncePlayerAchievements
    , difficulty                    :: !Difficulty
    , enableCommandBlock            :: !Difficulty
    , enableQuery                   :: !EnableQuery
    , enableRcon                    :: !EnableRcon
    , forceGameMode                 :: !ForceGameMode
    , gamemode                      :: !GameMode
    , generateStructures            :: !GenerateStructures
    , generatorSettings             :: !GeneratorSettings
    , hardcore                      :: !Hardcore
    , levelName                     :: !LevelName
    , levelSeed                     :: !LevelSeed
    , levelType                     :: !LevelType
    , maxBuildHeight                :: !MaxBuildHeight
    , maxPlayers                    :: !MaxPlayers
    , maxTickTime                   :: !MaxTickTime
    , maxWorldSize                  :: !MaxWorldSize
    , motd                          :: !MOTD
    , networkCompressionThreshold   :: !NetworkCompressionThreshold
    , onlineMode                    :: !OnlineMode
    , opPermissionLevel             :: !OpPermissionLevel
    , playerIdleTimeout             :: !PlayerIdleTimeout
    , pvp                           :: !PVP
    , queryPort                     :: !QueryPort
    , rconPassword                  :: !RconPassword
    , rconPort                      :: !RconPort
    , resourcePack                  :: !ResourcePack
    , resourcePackHash              :: !ResourcePackHash
    , serverIP                      :: !ServerIP
    , serverPort                    :: !ServerPort
    , snooperEnabled                :: !SnooperEnabled
    , spawnAnimals                  :: !SpawnAnimals
    , spawnMonsters                 :: !SpawnMonsters
    , spawnNPCs                     :: !SpawnNPCs
    , spawnProtection               :: !SpawnProtection
    , useNativeTransport            :: !UseNativeTransport
    , viewDistance                  :: !ViewDistance
    , whiteList                     :: !WhiteList
    } deriving (Show,Eq,Read)

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------


readServerProperties :: FilePath -> IO (Either ParseError [(T.Text,T.Text)])
readServerProperties path = do
    string <- readFile path
    let result = parse serverPropertiesFile "(I can't even...)" string
    case result of
        Left err -> return $ Left err
        Right serverProperties -> return $ Right serverProperties


{-
writeServerProperties :: FilePath -> M.Map T.Text T.Text -> IO ()
writeServerProperties path config = writeFile path $ fromServerProperties config
    where fromServerProperties c = T.unpack $ T.unlines $ fmap mkLine $ M.toList c
-}

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------


serverPropertiesFile :: GenParser Char st [(T.Text,T.Text)]
serverPropertiesFile = do result <- many line
                          eof
                          return $ (fmap toText) $ (filterComments result)
    where filterComments ls = filter (\(a,_) -> ('#' /= head a)) ls
          toText (a,b) = (T.pack a, T.pack b)


line :: GenParser Char st (String,String)
line  = do result <- fields
           eol
           return result


fields :: GenParser Char st (String,String)
fields = do first <- fieldContent
            second <- valueContent
            return (first,second)


fieldContent :: GenParser Char st String
fieldContent = many (noneOf "=\n")


valueContent :: GenParser Char st String
valueContent = (char '=' >> fieldContent) <|> (return [])


eol :: GenParser Char st Char
eol = char '\n'


-------------------------------------------------------------------------------
-- ServerProperties Builder
-------------------------------------------------------------------------------

buildServerProperties :: [(T.Text,T.Text)] -> ServerProperties
buildServerProperties lst = do
        ServerProperties
            mkAllowFlight $ f "allow-flight" lst
            mkAllowNether $ f "allow-nether" lst
            mkAnnouncePlayerAchievements $ f "announce-player-achievements" lst
            mkDifficulty $ f "difficulty" lst
            mkEnableQuery $ f "enable-query" lst
            mkEnableRcon $ f "enable-rcon" lst
            mkEnableCommandBlock $ f "enable-command-block" lst
            mkForceGameMode $ f "force-gamemode" lst
            mkGameMode $ f "gamemode" lst
            mkGenerateStructures $ f "generate-structures" lst
            mkGeneratorSettings $ f "generator-settings" lst
            mkHardcore $ f "hardcore" lst
            mkLevelName $ f "level-name" lst
            mkLevelSeed $ f "level-seed" lst
            mkLevelType $ f "level-type" lst
            mkMaxBuildHeight $ f "max-build-height" lst
            mkMaxPlayers $ f "max-players" lst
            mkMaxTickTime $ f "max-tick-time" lst
            mkMaxWorldSize $ f "max-world-size" lst
            mkMOTD $ f "motd" lst
            mkNetworkCompressionThreshold $ f "network-compression-threshold" lst
            mkOnlineMode $ f "online-mode" lst
            mkOpPermissionLevel $ f "op-permission-level" lst
            mkPlayerIdleTimeout $ f "player-idle-timeout" lst
            mkPVP $ f "pvp" lst
            mkQueryPort $ f "query.port" lst
            mkRconPassword $ f "rcon.password" lst
            mkRconPort $ f "rcon.port" lst
            mkResourcePack $ f "resource-pack" lst
            mkResourcePackHash $ f "resource-pack-hash" lst
            mkServerIP $ f "server-ip" lst
            mkServerPort $ f "server-port" lst
            mkSnooperEnabled $ f "snooper-enabled" lst
            mkSpawnAnimals $ f "spawn-animals" lst
            mkSpawnMonsters $ f "spawn-monsters" lst
            mkSpawnNPCs $ f "spawn-npcs" lst
            mkSpawnProtection $ f "spawn-protection" lst
            mkUseNativeTransport $ f "use-native-transport" lst
            mkViewDistance $ f "view-distance" lst
            mkWhiteList $ f "white-list" lst
    where f str list = filter (==str) list
-------------------------------------------------------------------------------
-- Smart Constructors
-------------------------------------------------------------------------------


mkAllowFlight :: T.Text -> Either String AllowFlight
mkAllowFlight raw = do
        case raw of
            "true"  -> Right $ AllowFlight True
            "false" -> Right $ AllowFlight False
            ""      -> Right $ AllowFlight False
            _       -> Left err
    where err = "Error: allow-flight value invalid!"


mkAllowNether :: T.Text -> Either String AllowNether
mkAllowNether raw = do
        case raw of
            "true"  -> Right $ AllowNether True
            "false" -> Right $ AllowNether False
            ""      -> Right $ AllowNether False
            _       -> Left err
    where err = "Error: allow-nether value invalid!"


mkAnnouncePlayerAchievements :: T.Text -> Either String AnnouncePlayerAchievements
mkAnnouncePlayerAchievements raw = do
        case raw of
            "true"  -> Right $ AnnouncePlayerAchievements True
            "false" -> Right $ AnnouncePlayerAchievements False
            ""      -> Right $ AnnouncePlayerAchievements False
            _       -> Left err
    where err = "Error: announce-player-achievements value invalid!"


mkDifficulty :: T.Text -> Either String Difficulty
mkDifficulty raw = do
        case (readErr err (T.unpack raw) :: Either String Int) of
            Right n -> if (n >= 0 && n <= 3)
                          then Right $ Difficulty n
                          else Left err
            Left err -> Left err
    where err = "Error: difficulty value invalid!"


mkEnableCommandBlock :: T.Text -> Either String EnableCommandBlock
mkEnableCommandBlock raw = do
        case raw of
            "true"  -> Right $ EnableCommandBlock True
            "false" -> Right $ EnableCommandBlock False
            ""      -> Right $ EnableCommandBlock False
            _       -> Left err
    where err = "Error: enable-command-block value invalid!"


mkEnableQuery :: T.Text -> Either String EnableQuery
mkEnableQuery raw = do
        case raw of
            "true"  -> Right $ EnableQuery True
            "false" -> Right $ EnableQuery False
            ""      -> Right $ EnableQuery False
            _       -> Left err
    where err = "Error: enable-query value invalid!"


mkEnableRcon :: T.Text -> Either String EnableRcon
mkEnableRcon raw = do
        case raw of
            "true"  -> Right $ EnableRcon True
            "false" -> Right $ EnableRcon False
            ""      -> Right $ EnableRcon True
            _       -> Left err
    where err = "Error: enable-rcon value invalid!"


mkForceGameMode :: T.Text -> Either String ForceGameMode
mkForceGameMode  raw = do
        case raw of
            "true"  -> Right $ ForceGameMode True
            "false" -> Right $ ForceGameMode False
            ""      -> Right $ ForceGameMode False
            _       -> Left err
    where err = "Error: force-gamemode value invalid!"


mkGameMode :: T.Text -> Either String GameMode
mkGameMode raw = do
        case (readErr err (T.unpack raw) :: Either String Int) of
             Right n -> if (n >= 0 && n <= 3)
                           then Right $ GameMode n
                           else Left err
             Left err -> Left err
    where err = "Error: gamemode value invalid!"


mkGenerateStructures :: T.Text -> Either String GenerateStructures
mkGenerateStructures raw = do
        case raw of
            "true"  -> Right $ GenerateStructures True
            "false" -> Right $ GenerateStructures False
            ""      -> Right $ GenerateStructures False
            _       -> Left err
    where err = "Error: generate-structure value invalid!"


-- TODO! (Michael) Parse generator settings correctly.
mkGeneratorSettings :: T.Text -> Either String GeneratorSettings
mkGeneratorSettings raw = GeneratorSettings raw


mkHardcore :: T.Text -> Either String Hardcore
mkHardcore raw = do
        case raw of
            "true"  -> Right $ Hardcore True
            "false" -> Right $ Hardcore False
            ""      -> Right $ Hardcore False
            _       -> Left err
    where err = "Error: hardcore value invalid!"


mkLevelName :: T.Text -> Either String LevelName
mkLevelName raw = LevelName raw


mkLevelSeed :: T.Text -> Either String LevelSeed
mkLevelSeed raw = LevelSeed raw


mkLevelType :: T.Text -> Either String LevelType
mkLevelType raw = do
        case raw of
            "DEFAULT"       -> Right $ LevelType raw
            "FLAT"          -> Right $ LevelType raw
            "LARGEBIOMES"   -> Right $ LevelType raw
            "AMPLIFIED"     -> Right $ LevelType raw
            "CUSTOMIZED"    -> Right $ LevelType raw
            ""              -> Right $ LevelType raw
            _               -> Left err
    where err = "Error: level-type value invalid!"


mkMaxBuildHeight :: T.Text -> Either String MaxBuildHeight
mkMaxBuildHeight n = if n <= 256
                        then Right $ MaxBuildHeight n
                        else Left err
    where err = "Error: max-build-height value invalid!"


mkMaxPlayers :: T.Text -> Either String MaxPlayers
mkMaxPlayers n = if (n >= 0 && n <= 2147483647)
                    then Right $ MaxPlayers n
                    else Left err
    where err = "Error: max-players value invalid!"


mkMaxTickTime :: T.Text -> Either String MaxTickTime
mkMaxTickTime n = if n >= 0
                     then Right $ MaxTickTime n
                     else Left err
    where err = "Error: max-tick-time value invalid!"


mkMaxWorldSize :: T.Text -> Either String MaxWorldSize
mkMaxWorldSize n = if (n >= 1 && n <= 29999984)
                      then Right $ MaxWorldSize n
                      else Left err
    where err = "Error: max-world-size value invalid!"


mkMOTD :: T.Text -> Either String MOTD
mkMOTD t = MOTD t


mkNetworkCompressionThreshold :: T.Text -> Either String NetworkCompressionThreshold
mkNetworkCompressionThreshold n = if (n >= -1 && n <= 256)
                                     then Right $ NetworkCompressionThreshold n
                                     else Left err
    where err = "Error: network-compression-threshold value invalid!"


mkOnlineMode :: T.Text -> Either String OnlineMode
mkOnlineMode raw = do
        case raw of
            "true"  -> Right $ OnlineMode True
            "false" -> Right $ OnlineMode False
            ""      -> Right $ OnlineMode False
            _       -> Left err
    where err = "Error: online-mode value invalid!"


mkOpPermissionLevel :: T.Text -> Either String OpPermissionLevel
mkOpPermissionLevel n = if (n >= 1 && n <= 4)
                           then Right $ OpPermissionLevel n
                           else Left err
    where err = "Error: op-permission-level value invalid!"


mkPlayerIdleTimeout :: T.Text -> Either String PlayerIdleTimeout
mkPlayerIdleTimeout n = if n >= 0
                           then Right $ PlayerIdleTimeout n
                           else Left err
    where err = "Error: player-idle-timeout value invalid!"


mkPVP :: T.Text -> Either String PVP
mkPVP raw = do
        case raw of
            "true"  -> Right $ PVP True
            "false" -> Right $ PVP False
            ""      -> Right $ PVP False
            _       -> Left err
    where err = "Error: pvp value invalid!"


mkQueryPort :: T.Text -> Either String QueryPort
mkQueryPort n = if (n >= 1 && n <= 65534)
                   then Right $ QueryPort n
                   else Left "Error: query.port value invalid!"


mkRconPassword :: T.Text -> Either String RconPassword
mkRconPassword t = RconPassword t


mkRconPort :: T.Text -> Either String RconPort
mkRconPort n = if (n >= 1 && n <= 65534)
                  then Right $ RconPort n
                  else Left "Error: rcon.port value invalid!"


mkResourcePack :: T.Text -> Either String ResourcePack
mkResourcePack t = ResourcePack t


mkResourcePackHash :: T.Text -> Either String ResourcePackHash
mkResourcePackHash t = ResourcePackHash t


mkServerIP :: T.Text -> Either String ServerIP
mkServerIP ip = ServerIP ip


mkServerPort :: T.Text -> Either String ServerPort
mkServerPort n = if (n >= 1 && n <= 65534)
                    then Right $ ServerPort n
                    else Left "Error: server-port value invalid!"


mkSnooperEnabled :: T.Text -> Either String SnooperEnabled
mkSnooperEnabled raw = do
        case raw of
            "true"  -> Right $ SnooperEnabled True
            "false" -> Right $ SnooperEnabled False
            ""      -> Right $ SnooperEnabled False
            _       -> Left err
    where err = "Error: snooper-enabled value invalid!"


mkSpawnAnimals :: T.Text -> Either String SpawnAnimals
mkSpawnAnimals raw = do
        case raw of
            "true"  -> Right $ SpawnAnimals True
            "false" -> Right $ SpawnAnimals False
            ""      -> Right $ SpawnAnimals False
            _       -> Left err
    where err = "Error: spawn-animals value invalid!"


mkSpawnMonsters :: T.Text -> Either String SpawnMonsters
mkSpawnMonsters raw = do
        case raw of
            "true"  -> Right $ SpawnMonsters True
            "false" -> Right $ SpawnMonsters False
            ""      -> Right $ SpawnMonsters False
            _       -> Left err
    where err = "Error: spawn-monsters value invalid!"


mkSpawnNPCs :: T.Text -> Either String SpawnNPCs
mkSpawnNPCs raw = do
        case raw of
            "true"  -> Right $ SpawnNPCs True
            "false" -> Right $ SpawnNPCs False
            ""      -> Right $ SpawnNPCs False
            _       -> Left err
    where err = "Error: spawn-npcs value invalid!"


mkSpawnProtection :: T.Text -> Either String SpawnProtection
mkSpawnProtection raw = do
        let x = (readErr err (T.unpack raw) :: Either String Int)
        case x of
            Right n -> if n >= 0
                          then Right $ SpawnProtection n
                          else Left err
            Left s -> Left err
    where err = "Error: spawn-protection value invalid!"


mkUseNativeTransport :: T.Text -> Either String UseNativeTransport
mkUseNativeTransport raw = do
        case raw of
            "true"  -> Right $ UseNativeTransport True
            "false" -> Right $ UseNativeTransport False
            ""      -> Right $ UseNativeTransport False
            _       -> Left err
    where err = "Error: use-native-transport value invalid!"

mkViewDistance :: T.Text -> Either String ViewDistance
mkViewDistance raw = do
    let x = (readErr err (T.unpack raw) :: Either String Int)
    case x of
        Right n -> if (n >= 3 && n <= 15)
                      then Right $ ViewDistance n
                      else Left err
        Left s -> Left err
    where err = "Error: view-distance value invalid!"


mkWhiteList :: T.Text -> Either String WhiteList
mkWhiteList raw = do
        case raw of
            "true"  -> Right $ WhiteList True
            "false" -> Right $ WhiteList False
            ""      -> Right $ WhiteList False
            _       -> Left err
    where err = "Error: whitelist value invalid!"


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------


mkLine :: (T.Text,T.Text) -> T.Text
mkLine (a,b) = T.concat [a,"=",b]


extractBool :: String -> Bool
extractBool r = do
    case r of
        "true"  -> True
        "false" -> False
        _       -> undefined


-------------------------------------------------------------------------------
