-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Tmux
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (requires Tmux)
--
-- Open Sandbox functions and types for controlling and manipulating Tmux.
-------------------------------------------------------------------------------
module OpenSandbox.Tmux
    ( TmuxID
    , TmuxSessionID
    , TmuxWindowID
    , tmuxID
    , fmtTmuxID
    , parseTmuxID
    , sendTmux
    , detachClient
    , newWindow
    , killWindow
    , isRunning
    , tmuxInit
    , tmuxClose
    ) where

import Control.Monad
import Data.Functor.Identity
import OpenSandbox.Service
import System.Directory
import System.Exit
import System.Process
import Test.QuickCheck


newtype TmuxID = TmuxID (TmuxSessionID,TmuxWindowID) deriving (Show,Read,Eq,Ord)

instance Arbitrary TmuxID where
  arbitrary = do
    let s = arbitrary :: Gen String
    let w = arbitrary :: Gen String
    liftM (TmuxID) $ liftM2 (,) s w

type TmuxSessionID = String
type TmuxWindowID = String


tmuxID :: Service -> TmuxID
tmuxID s = TmuxID $ ("opensandbox",(show $ srvPort s))


fmtTmuxID :: TmuxID -> String
fmtTmuxID (TmuxID (s,w)) = s ++ ":" ++ w


-- | Takes a String `"sessionID" ++ ":" ++ "windowID"` and might build a TmuxID out of it.
--
-- > parseTmuxID "opensandbox:25565" = Just (TmuxID ("opensandbox","25565"))
--
-- It will return `Nothing` if it is given a string that has more or less than 1 ':' present in it.
--
-- > parseTmuxID "opensandbox25565" = Nothing
--
-- It will also return `Nothing` if either the sessionID or the windowID are an empty string.
--
-- > parseTmuxID ":25565" = Nothing
-- > parseTmuxID "opensandbox:" = Nothing
parseTmuxID :: String -> Maybe TmuxID
parseTmuxID x = if cnt ':' x == 1
                  then if s /= [] && w /= []
                          then Just (TmuxID (s,w))
                          else Nothing
                  else Nothing
  where s = fst $ break (==':') x
        w = tail . snd $ break (==':') x
        cnt i lst = length $ filter (==i) lst


sendTmux :: TmuxID -> String -> IO ()
sendTmux t c = callCommand $ "tmux send -t " ++ fmtTmuxID t ++ " " ++ show c ++ " ENTER"


detachClient :: TmuxWindowID -> IO ()
detachClient w = callCommand $ "tmux detach-client -t " ++ w


newWindow :: TmuxID -> FilePath -> String -> IO ()
newWindow t d n = callCommand $ "tmux new-window -t " ++ fmtTmuxID t ++ " -c " ++ d ++ " -n " ++ n


killWindow :: TmuxWindowID -> IO ()
killWindow w = callCommand $ "tmux kill-window -t " ++ w


isRunning :: IO Bool
isRunning = doesDirectoryExist "/tmp/tmux-1000"


tmuxInit :: IO ()
tmuxInit = do
  putStr "Starting tmux session: "
  p <- spawnCommand "tmux new -d -s opensandboxd"
  code <- waitForProcess p
  case code of
    ExitSuccess -> putStrLn "[Success]"
    ExitFailure i -> putStrLn "[Fail]"


tmuxClose :: IO ()
tmuxClose = do
  putStr "Closing tmux..."
  p <- spawnCommand "tmux kill-session -t opensandboxd"
  code <- waitForProcess p
  case code of
    ExitSuccess -> putStrLn "[Success]"
    ExitFailure i -> putStrLn "[Fail]"
