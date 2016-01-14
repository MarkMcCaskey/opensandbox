{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Op
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Op
    ( Op
    , readOps
    , writeOps
    , addOp
    , rmOp
    , modOp
    ) where


import            Control.Applicative
import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString.Lazy as B
import            Data.Set
import qualified  Data.Text as T
import            Data.UUID
import            Data.UUID.Aeson
import            GHC.Generics
import qualified  OpenSandbox.User as U


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
addOp opset U.User {U.userUUID = u, U.userName = n} lvl bypass =
    insert (Op u n lvl bypass) opset


rmOp :: Set Op -> Op -> Set Op
rmOp opset op = delete op opset


modOp :: Set Op -> Op -> Int -> Bool -> Set Op
modOp opset op lvl bypass = insert newOp (rmOp opset op)
    where newOp = Op (uuid op) (name op) lvl bypass
