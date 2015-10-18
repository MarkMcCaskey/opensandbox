import OpenSandbox

main :: IO ()
main = do
    putStrLn "Ops are: "
    ops <- readOps "ops.json"
    case ops of
      Left err -> putStrLn err
      Right opsList -> print opsList

    putStrLn "Users are: "
    users <- readUsers "usercache.json"
    case users of
      Left err -> putStrLn err
      Right userlist -> do
        print userlist
        writeUsers "test.json" userlist

    putStrLn "Whitelisted Users are: "
    whitelist <- readWhiteList "whitelist.json"
    case whitelist of
      Left err -> putStrLn err
      Right whitelist -> print whitelist

    putStrLn "Banned IPs are: "
    bannedIPs <- readBannedIPs "banned-ips.json"
    case bannedIPs of
      Left err -> putStrLn err
      Right bannedIPs -> print bannedIPs

    putStrLn "Banned Players are: "
    bannedPlayers <- readBannedPlayers "banned-players.json"
    case bannedPlayers of
      Left err -> putStrLn err
      Right bannedPlayers -> print bannedPlayers
