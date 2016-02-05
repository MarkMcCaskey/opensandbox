-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Effect
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Effect
  ( Effect (..)
  , EffectType (..)
  ) where

import qualified  Data.Text as T


data Effect = Effect
  { effectID            :: !Int
  , effectName          :: !T.Text
  , effectDisplayName   :: !T.Text
  , effectType          :: !EffectType
  } deriving (Show,Eq)


data EffectType = GoodEffect | BadEffect deriving (Show,Eq)
