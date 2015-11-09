-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Tmux
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Tmux
    ( tmuxInit
    , tmuxClose
    ) where


import Data.Functor.Identity
import System.Exit
import System.Process


newtype TmuxCommand = Tmux String deriving (Show,Eq,Read)


type Tmux s w = TmuxT s w Identity


newtype TmuxT s w m a = TmuxT { runTmuxT :: s -> w -> m a }


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


createSession :: String -> TmuxCommand
createSession session = Tmux $ "new -d -s " ++ session


attachSession :: String -> TmuxCommand
attachSession session = Tmux $ "a -t " ++ session


detachSession :: String -> TmuxCommand
detachSession session = Tmux $ "a -t " ++ session


killSession :: String -> TmuxCommand
killSession session = Tmux $ "kill-session -t " ++ session


killServer :: TmuxCommand
killServer = Tmux "kill-server"


newWindow :: String -> Int -> TmuxCommand
newWindow session windowID = Tmux $ "new-window -t " ++ session ++ ":" ++ (show windowID)
