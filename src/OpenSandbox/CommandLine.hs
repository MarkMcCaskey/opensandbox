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
  ( getOpts
  , opensandboxOpts
  , debugFlag
  , verboseFlag
  ) where

import          Options.Applicative
import          OpenSandbox.Logger (Lvl (..))


type Debug = Lvl

type Verbose = Bool


getOpts :: ParserInfo a -> IO a
getOpts = execParser


opensandboxOpts :: ParserInfo (Maybe Debug)
opensandboxOpts =
  info (helper <*> debugFlag)
    ( fullDesc
   <> progDesc "The OpenSandbox server"
   <> header "opensandbox - an opensandbox server" )


debugFlag :: Parser (Maybe Debug)
debugFlag = optional $ flag Info Debug
  ( long "debug"
  <> short 'd'
  <> help "Enable debugging" )


verboseFlag :: Parser (Maybe Verbose)
verboseFlag = optional $ flag False True
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose mode")
