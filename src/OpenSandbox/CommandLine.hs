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
  { getCustomRootDir    :: Maybe FilePath
  , getCustomConfigDir  :: Maybe FilePath
  , getCustomLogDir     :: Maybe FilePath
  , getDebugFlag        :: Bool
  , getVersionFlag      :: Bool
  , getVerboseFlag      :: Bool
  , getDaemonFlag       :: Bool
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
  <$> (optional $ strOption
    ( long "rootDir"
    <> metavar "DIR"
    <> help "Select an alternative root dir for OpenSandbox to start in (DEFAULT: ./)"))
  <*> (optional $ strOption
    ( long "configDir"
    <> metavar "DIR"
    <> help "Select an alternative config dir (DEFAULT: ./config/)"))
  <*> (optional $ strOption
    ( long "logDir"
    <> metavar "DIR"
    <> help "Select an alternative log dir (DEFAULT: ./logs)"))
  <*> switch
    ( long "debug"
    <> help "Enables Debug logging." )
  <*> switch
    ( long "version"
    <> help "Print OpenSandbox version and exit.")
  <*> switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable Verbose logging.")
  <*> switch
    ( long "daemon"
    <> short 'd'
    <> help "Run as daemon in the background.")
