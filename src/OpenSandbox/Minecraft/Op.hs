{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Copyright        : (c) 2015 Michael Carpenter
-- License          : BSD3
-- Maintainer       : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability        : provisional
-- Portability      : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Op (
    Op,
    readOps,
    writeOps,
    addOp,
    rmOp,
    modOp
) where

-- External Imports
import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Set
import qualified Data.Text as T
import Data.UUID
import Data.UUID.Aeson
import GHC.Generics

-- Internal Imports
import qualified OpenSandbox.Minecraft.User as U


data Op = Op
  { uuid                 :: !UUID
  , name                 :: !T.Text
  , level                :: !Int
  , bypassesPlayerLimit  :: !Bool
  } deriving (Show,Ord,Eq,Read,Generic)


instance FromJSON Op
instance ToJSON Op


readOps :: FilePath -> IO (Either String (Set Op))
readOps path = eitherDecode <$> B.readFile path


writeOps :: FilePath -> Set Op -> IO ()
writeOps path ops = do
    let json = encode ops
    B.writeFile path json


addOp :: Set Op -> U.User -> Int -> Bool -> Set Op
addOp opset U.User {U.uuid = u, U.name = n} lvl bypass =
    insert (Op u n lvl bypass) opset


rmOp :: Set Op -> Op -> Set Op
rmOp opset op = delete op opset


modOp :: Set Op -> Op -> Int -> Bool -> Set Op
modOp opset op lvl bypass = insert newOp (rmOp opset op)
    where newOp = Op (uuid op) (name op) lvl bypass
