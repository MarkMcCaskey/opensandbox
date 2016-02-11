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
  , opensandboxOpts
  , debugFlag
  , verboseFlag
  , versionFlag
  ) where

import          Options.Applicative


data OpenSandboxOption
  = DebugFlag Bool
  | VerboseFlag Bool
  | VersionFlag Bool
  deriving (Show,Eq)


getOpts :: ParserInfo a -> IO a
getOpts = execParser


opensandboxOpts :: ParserInfo [OpenSandboxOption]
opensandboxOpts =
  info (some (debugFlag <|> verboseFlag <|> versionFlag))
    ( fullDesc
   <> progDesc "The OpenSandbox server"
   <> header "opensandbox - an opensandbox server" )


debugFlag :: Parser OpenSandboxOption
debugFlag = DebugFlag
  <$> switch
  ( long "debug"
  <> short 'd'
  <> help "Enable debugging" )


verboseFlag :: Parser OpenSandboxOption
verboseFlag = VerboseFlag
  <$> switch
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose mode")


versionFlag :: Parser OpenSandboxOption
versionFlag = VersionFlag
  <$> switch
  ( long "version"
  <> help "Print OpenSandbox version and exit.")
