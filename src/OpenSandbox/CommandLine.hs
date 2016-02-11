-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.CommandLine
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.CommandLine
  ( OpenSandboxOption (..)
  , getOpts
  , openSandboxOpts
  , openSandboxOptions
  ) where

import Options.Applicative


data OpenSandboxOption = OpenSandboxOption
  { getDebugFlag    :: Bool
  , getVersionFlag  :: Bool
  } deriving (Show,Eq)


getOpts :: IO OpenSandboxOption
getOpts = execParser openSandboxOpts


openSandboxOpts :: ParserInfo OpenSandboxOption
openSandboxOpts =
  info (helper <*> openSandboxOptions)
    ( fullDesc
   <> progDesc "The OpenSandbox server"
   <> header "opensandbox - an opensandbox server" )


openSandboxOptions :: Parser OpenSandboxOption
openSandboxOptions = OpenSandboxOption
  <$> switch
    ( long "debug"
    <> short 'd'
    <> help "Enable debugging" )
  <*> switch
    ( long "version"
    <> short 'v'
    <> help "Print OpenSandbox version and exit.")
