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
    ( TmuxID
    , MCCommand
    , sendTmux
    , tmuxInit
    , tmuxClose
    ) where


import Data.Functor.Identity
import System.Exit
import System.Process


type MCCommand = String


type TmuxID = String


sendTmux :: TmuxID -> MCCommand -> IO ()
sendTmux t c = callCommand $ "tmux send -t " ++ (show t) ++ " " ++ (show c) ++ " ENTER"


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
