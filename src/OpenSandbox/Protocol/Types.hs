{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Types
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Types
  ( Chat
  , Short
  , Angle
  , Position
  , Animation (..)
  , BossBarAction (..)
  , EntityMetadataEntry (..)
  , ValueField (..)
  , EntityMetadata
  , PlayerProperty (..)
  , PlayerListAction (..)
  , Stat (..)
  , Player (..)
  , StatusPayload (..)
  , Players (..)
  , Version (..)
  , Description (..)
  , BlockChange (..)
  , Icon (..)
  , CombatEvent (..)
  , WorldBorderAction (..)
  , TeamMode (..)
  , TitleAction (..)
  , Slot
  , EntityProperty (..)
  , ChunkSection (..)
  , putVarInt
  , getVarInt
  , putByteStringField
  , putPosition
  , putText
  , putBool
  , putSlot
  , putUUID
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import            Data.Int
import            Data.Maybe
import            Data.NBT
import            Data.Serialize
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word
import            GHC.Generics

import OpenSandbox.Types

type Chat = T.Text

type Short = Word16

type Angle = Word8

type Position = Word64

data BlockChange = BlockChange
  { hPosition     :: !Word8
  , yCoord        :: !Word8
  , blockId       :: !Int
  } deriving (Show,Eq)

data Animation
  = SwingArm
  | TakeDamage
  | LeaveBed
  | EatFood
  | CriticalEffect
  | MagicCriticalEffect
  deriving (Show,Eq)

instance Enum Animation where
  fromEnum SwingArm = 0
  fromEnum TakeDamage = 1
  fromEnum LeaveBed = 2
  fromEnum EatFood = 3
  fromEnum CriticalEffect = 4
  fromEnum MagicCriticalEffect = 5
  toEnum 0 = SwingArm
  toEnum 1 = TakeDamage
  toEnum 2 = LeaveBed
  toEnum 3 = EatFood
  toEnum 4 = CriticalEffect
  toEnum 5 = MagicCriticalEffect
  toEnum _ = undefined

data BossBarAction
  = BossBarAdd Chat Float Int Int Word8
  | BossBarRemove
  | BossBarUpdateHealth Float
  | BossBarUpdateTitle Chat
  | BossBarUpdateStyle Int Int
  | BossBarUpdateFlags Word8
  deriving (Show,Eq)

data EntityMetadataEntry = Entry
  { entryIndex  :: Word8
  , entryType   :: Maybe Word8
  , entryValue  :: Maybe Word8
  } deriving (Show,Eq)

data ValueField
  = ByteField
  | VarIntField
  | FloatField
  | StringField
  | ChatField
  | SlotField
  | BoolField
  | Vector3FField
  | PositionField
  | OptPositionField
  | DirectionField
  | OptUUIDField
  | BlockIDField
  deriving (Show,Eq)

instance Enum ValueField where
  fromEnum ByteField = 0
  fromEnum VarIntField = 1
  fromEnum FloatField = 2
  fromEnum StringField = 3
  fromEnum ChatField = 4
  fromEnum SlotField = 5
  fromEnum BoolField = 6
  fromEnum Vector3FField = 7
  fromEnum PositionField = 8
  fromEnum OptPositionField = 9
  fromEnum DirectionField = 10
  fromEnum OptUUIDField = 11
  fromEnum BlockIDField = 12
  toEnum 0 = ByteField
  toEnum 1 = VarIntField
  toEnum 2 = FloatField
  toEnum 3 = StringField
  toEnum 4 = ChatField
  toEnum 5 = SlotField
  toEnum 6 = BoolField
  toEnum 7 = Vector3FField
  toEnum 8 = PositionField
  toEnum 9 = OptPositionField
  toEnum 10 = DirectionField
  toEnum 11 = OptUUIDField
  toEnum 12 = BlockIDField
  toEnum _ = undefined

type EntityMetadata = V.Vector EntityMetadataEntry

data StatusPayload = StatusPayload
  { version       :: Version
  , players       :: Players
  , description   :: Description
  } deriving (Generic,Show,Eq,Read)


instance Aeson.ToJSON StatusPayload
instance Aeson.FromJSON StatusPayload


data Version = Version
  { name      :: T.Text
  , protocol  :: Word8
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Version
instance Aeson.FromJSON Version


data Players = Players
  { max     :: Word8
  , online  :: Word8
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Players
instance Aeson.FromJSON Players


data Description = Description
  { text    :: T.Text
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Description
instance Aeson.FromJSON Description

data ChunkSection = ChunkSection
  { bitsPerBlock  :: !Word8
  , palette       :: !(Maybe (V.Vector Int))
  , dataArray     :: !B.ByteString
  , blockLight    :: !B.ByteString
  , skyLight      :: !(Maybe B.ByteString)
  } deriving (Show,Eq)

data Stat = Stat T.Text Int deriving (Show,Eq)


instance Serialize Stat where
  put (Stat statName statVal) = do
    putByteStringField . encodeUtf8 $ statName
    putVarInt statVal

  get = Stat <$> fmap decodeUtf8 (getVarInt >>= getByteString) <*> getVarInt


instance (Serialize a) => Serialize (V.Vector a) where
  put v = do
    putVarInt . V.length $ v
    (mapM_ put v)

  get = undefined


data Player = Player
  { playerUUID        :: UUID
  , playerListAction  :: PlayerListAction
  } deriving (Show,Eq)


instance Serialize Player where
  put (Player u pla) = do
    putByteString . BL.toStrict . toByteString $ u
    put pla

  get = undefined


data PlayerListAction
  = PlayerListAdd T.Text (V.Vector PlayerProperty) GameMode Int (Maybe T.Text)
  | PlayerListUpdateGameMode GameMode
  | PlayerListUpdateLatency Int
  | PlayerListUpdateDisplayName Bool (Maybe T.Text)
  | PlayerListRemovePlayer
  deriving (Show,Eq)


instance Serialize PlayerListAction where
  put (PlayerListAdd name properties gameMode ping displayName) = do
    let namePayload = encodeUtf8 name
    let nameLen = B.length namePayload
    putVarInt nameLen
    putByteString namePayload
    put properties
    putVarInt . fromEnum $ gameMode
    putVarInt ping
    if displayName /= Nothing
      then do
        let displayPayload = encodeUtf8 . fromJust $ displayName
        putVarInt . B.length $ displayPayload
        putByteString displayPayload
      else
        return ()
  put (PlayerListUpdateGameMode gameMode) = do
    putVarInt . fromEnum $ gameMode
  put (PlayerListUpdateLatency ping) = do
    putVarInt ping
  put (PlayerListUpdateDisplayName hasDisplayName displayName) = do
    put hasDisplayName
    if displayName /= Nothing
      then do
        let displayPayload = encodeUtf8 . fromJust $ displayName
        putVarInt . B.length $ displayPayload
        putByteString displayPayload
      else
        return ()
  put PlayerListRemovePlayer = return ()
  get = undefined


data PlayerProperty = PlayerProperty
  { playerName    :: !T.Text
  , playerValue   :: !T.Text
  , isSigned      :: !Bool
  , playerSig     :: !(Maybe T.Text)
  } deriving (Show,Eq)


instance Serialize PlayerProperty where
  put (PlayerProperty pn pv is ps) = do
    let pnPayload = encodeUtf8 pn
    putVarInt . B.length $ pnPayload
    putByteString pnPayload
    let pvPayload = encodeUtf8 pv
    putVarInt . B.length $ pvPayload
    putByteString pvPayload
    put is
    if ps /= Nothing
      then do
        let sigPayload = encodeUtf8 . fromJust $ ps
        putVarInt . B.length $ sigPayload
        putByteString sigPayload
      else do
        return ()

  get = undefined

data Icon = Icon
  { directionAndType  :: !Word8
  , x                 :: !Word8
  , z                 :: !Word8
  } deriving (Show,Eq)

data CombatEvent
  = EnterCombat
  | EndCombat Int Int32
  | EntityDead Int Int32 Chat
  deriving (Show,Eq)

data WorldBorderAction
  = SetSize Double
  | LerpSize Double Double Int64
  | SetCenter Double Double
  | Initialize Double Double Double Double Int64 Int Int Int
  | SetWarningTime Int
  | SetWarningBlocks Int
  deriving (Show,Eq)

data TeamMode
  = CreateTeam T.Text T.Text T.Text Word8 T.Text T.Text Word8 Int (V.Vector T.Text)
  | RemoveTeam
  | UpdateTeamInfo T.Text T.Text T.Text Word8 T.Text T.Text Word8
  | AddPlayers (V.Vector T.Text)
  | RemovePlayers (V.Vector T.Text)
  deriving (Show,Eq)

data TitleAction
  = SetTitle Chat
  | SetSubtitle Chat
  | SetTimesAndDisplay Int32 Int32 Int32
  | Hide
  | Reset
  deriving (Show,Eq)

type Slot = NBT

data EntityProperty = EntityProperty
  { key             :: !T.Text
  , value           :: !Double
  , numOfModifiers  :: !Int
  , modifiers       :: !Int
  } deriving (Show,Eq)

-- Adapted from the protocol-buffers library, but only for Serialize and Ints

putVarInt :: Int -> Put
putVarInt i | i < 0x80 = putWord8 (fromIntegral i)
            | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> putVarInt (i `shiftR` 7)
{-# INLINE putVarInt #-}

getVarInt :: Get Int
getVarInt = do
    w <- getWord8
    if testBit w 7 then go 7 (fromIntegral (w .&. 0x7F))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- getWord8
      if testBit w' 7 then go (n+7) (val .|. ((fromIntegral (w' .&. 0x7F)) `shiftL` n))
        else return (val .|. ((fromIntegral w') `shiftL` n))
{-# INLINE getVarInt #-}

-------------------------------------------------------------------------------

putByteStringField :: Serialize a => a -> PutM ()
putByteStringField x = do
  let payload = runPut (put x)
  let len = B.length payload
  if len /= 0
    then do putVarInt len
            putByteString payload
    else do putVarInt len
{-# INLINE putByteStringField #-}

putPosition :: Position -> PutM ()
putPosition p = putWord64be p
{-# INLINE putPosition #-}

putText :: T.Text -> PutM ()
putText = putByteString . encodeUtf8
{-# INLINE putText #-}

putBool :: Bool -> PutM ()
putBool b = put b
{-# INLINE putBool #-}

putSlot :: Slot -> PutM ()
putSlot = put
{-# INLINE putSlot #-}

putUUID :: UUID -> PutM ()
putUUID = putByteString . toASCIIBytes
{-# INLINE putUUID #-}
