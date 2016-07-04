{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Packet
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Packet
  ( SBHandshaking (..)
  , CBStatus (..)
  , SBStatus (..)
  , CBLogin (..)
  , SBLogin (..)
  , CBPlay (..)
  , SBPlay (..)
  , encodeSBHandshaking
  , decodeSBHandshaking
  , encodeCBStatus
  , decodeCBStatus
  , encodeSBStatus
  , decodeSBStatus
  , encodeCBLogin
  , decodeCBLogin
  , encodeSBLogin
  , decodeSBLogin
  , encodeCBPlay
  , decodeCBPlay
  , encodeSBPlay
  , decodeSBPlay
  , debugNetCode
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import qualified  Data.Attoparsec.ByteString as Decode
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as Encode
import            Data.Int
import            Data.Monoid
import qualified  Data.Text as T
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word

import            OpenSandbox.Protocol.Types

debugNetCode :: CBPlay -> IO ()
debugNetCode packet = do
  let encoded = BL.toStrict . Encode.toLazyByteString $ encodeCBPlay packet
  let decoded = Decode.parseOnly decodeCBPlay encoded :: Either String CBPlay
  putStrLn "==================================================================="
  putStrLn $ "Packet: " ++ show packet
  putStrLn "-------------------------------------------------------------------"
  putStrLn $ "Encoded:"
  putStrLn $ show encoded
  putStrLn "-------------------------------------------------------------------"
  putStrLn $ "Decoded:"
  putStrLn $ show decoded
  putStrLn "-------------------------------------------------------------------"
  putStrLn $ "Should be:"
  putStrLn $ show (Right packet :: Either String CBPlay)
  putStrLn "==================================================================="

data SBHandshaking
  -- | __Handshake:__
  -- This causes the server to switch into the target state.
  = SBHandshake VarInt T.Text Short NextState

  -- | __Legacy Server List Ping:__
  -- While not technically part of the current protocol, legacy clients may send this packet to initiate Server List Ping, and modern servers should handle it correctly.
  | SBLegacyServerListPing Word8
  deriving (Show,Eq)


encodeSBHandshaking :: SBHandshaking -> Encode.Builder
encodeSBHandshaking (SBHandshake protocolVersion srvAddress srvPort nextState) =
    Encode.word8 0x00
    <> encodeVarInt protocolVersion
    <> encodeText srvAddress
    <> Encode.int16BE srvPort
    <> (encodeVarInt . toEnum . fromEnum $ nextState)
encodeSBHandshaking (SBLegacyServerListPing payload) =
    Encode.word8 0xfe
    <> Encode.word8 payload


decodeSBHandshaking :: Decode.Parser SBHandshaking
decodeSBHandshaking = do
    packetID <- Decode.anyWord8
    case packetID of
      0x00  -> do
        protocolVersion <- decodeVarInt
        srvAddress <- decodeText
        srvPort <- decodeInt16BE
        nextState <- (fmap (toEnum . fromEnum) decodeVarInt)
        return $! SBHandshake protocolVersion srvAddress srvPort nextState
      0xfe  -> do
        payload <- Decode.anyWord8
        return $! SBLegacyServerListPing payload
      err -> fail $ "Unrecognized packetID: " ++ show err


data CBStatus
  = CBResponse T.Text Word8 Word8 Word8 T.Text
  | CBPong Int64
  deriving (Show,Eq)


encodeCBStatus :: CBStatus -> Encode.Builder
encodeCBStatus (CBResponse mcVersion versionID currentPlayers maxPlayers motd) =
    Encode.word8 0x00
    <> (encodeStatusPayload mcVersion versionID currentPlayers maxPlayers motd)
encodeCBStatus (CBPong payload) =
    Encode.word8 0x01
    <> Encode.int64BE payload


decodeCBStatus :: Decode.Parser CBStatus
decodeCBStatus = do
    packetID <- Decode.anyWord8
    case packetID of
      0x00  -> do
        rawJSON <- decodeByteString
        let eitherJSON = Aeson.eitherDecodeStrict rawJSON
        case eitherJSON of
          Left err -> fail err
          Right json -> do
            return $ CBResponse
              (name . version $ json)
              (protocol . version $ json)
              (online . players $ json)
              (max . players $ json)
              (text . description $ json)
      0x01  -> do
        payload <- decodeInt64BE
        return $ CBPong payload
      err -> fail $ "Unrecognized packetID: " ++ show err


data SBStatus
  = SBRequest
  | SBPing Int64
  deriving (Show,Eq)


encodeSBStatus :: SBStatus -> Encode.Builder
encodeSBStatus SBRequest =
  Encode.word8 0x00
encodeSBStatus (SBPing payload) =
  Encode.word8 0x01
  <> Encode.int64BE payload


decodeSBStatus :: Decode.Parser SBStatus
decodeSBStatus = do
    packetID <- Decode.anyWord8
    case packetID of
      0x00  -> do
        return $ SBRequest
      0x01  -> do
        payload <- decodeInt64BE
        return $ SBPing payload
      err -> fail $ "Unrecognized packetID: " ++ show err



data CBLogin
  = CBLoginDisconnect T.Text
  | CBEncryptionRequest T.Text B.ByteString B.ByteString
  | CBLoginSuccess UUID T.Text
  | CBSetCompression VarInt
  deriving (Show,Eq)


encodeCBLogin :: CBLogin -> Encode.Builder
encodeCBLogin (CBLoginDisconnect reason) =
  Encode.word8 0x00
  <> encodeText reason
encodeCBLogin (CBEncryptionRequest srvID publicKey verifyToken) =
  Encode.word8 0x01
  <> encodeText srvID
  <> (encodeVarInt . B.length $ publicKey)
  <> Encode.byteString publicKey
  <> (encodeVarInt . B.length $ verifyToken)
  <> Encode.byteString verifyToken
encodeCBLogin (CBLoginSuccess uuid username) =
  Encode.word8 0x02
  <> encodeUUID uuid
  <> encodeText username
encodeCBLogin (CBSetCompression threshold) =
  Encode.word8 0x03
  <> encodeVarInt threshold


decodeCBLogin :: Decode.Parser CBLogin
decodeCBLogin = do
    packetID <- Decode.anyWord8
    case packetID of
      0x00  -> do
        reason <- decodeText
        return $ CBLoginDisconnect reason
      0x01  -> do
        srvID <- decodeText
        publicKeyLn <- decodeVarInt
        publicKey <- Decode.take publicKeyLn
        verifyTokenLn <- decodeVarInt
        verifyToken <- Decode.take verifyTokenLn
        return $ CBEncryptionRequest srvID publicKey verifyToken
      0x02  -> do
        uuid <- decodeUUID
        username <- decodeText
        return $ CBLoginSuccess uuid username
      0x03  -> do
        threshold <- decodeVarInt
        return $ CBSetCompression threshold
      err -> fail $ "Unrecognized packetID: " ++ show err


data SBLogin
  = SBLoginStart T.Text
  | SBEncryptionResponse B.ByteString B.ByteString
  deriving (Show,Eq)


encodeSBLogin :: SBLogin -> Encode.Builder
encodeSBLogin (SBLoginStart name) =
  Encode.word8 0x00
  <> encodeText name
encodeSBLogin (SBEncryptionResponse sharedSecret verifyToken) =
  Encode.word8 0x01
  <> (encodeVarInt . B.length $ sharedSecret)
  <> Encode.byteString sharedSecret
  <> (encodeVarInt . B.length $ verifyToken)
  <> Encode.byteString verifyToken


decodeSBLogin :: Decode.Parser SBLogin
decodeSBLogin = do
    packetID <- Decode.anyWord8
    case packetID of
      0x00  -> do
        name <- decodeText
        return $ SBLoginStart name
      0x01  -> do
        sharedSecretLn <- decodeVarInt
        sharedSecret <- Decode.take sharedSecretLn
        verifyTokenLn <- decodeVarInt
        verifyToken <- Decode.take verifyTokenLn
        return $ SBEncryptionResponse sharedSecret verifyToken
      err -> fail $ "Unrecognized packetID: " ++ show err


data CBPlay

  -- | __Spawn Object:__
  -- Sent by the server when a vehicle or other object is created.
  = CBSpawnObject VarInt UUID Int8 Double Double Double Angle Angle Int32 Short Short Short

  -- | __Spawn Experience Orb:__
  -- Spawns one or more experience orbs.
  | CBSpawnExperienceOrb VarInt Double Double Double Int16

  -- | __Spawn Global Entity:__
  -- With this packet, the server notifies the client of thunderbolts striking within a 512 block radius around the player. The coordinates specify where exactly the thunderbolt strikes.
  | CBSpawnGlobalEntity VarInt Int8 Double Double Double

  -- | __Spawn Mob:__
  -- Sent by the server when a mob entity is spawned.
  | CBSpawnMob VarInt UUID Word8 Double Double Double Angle Angle Angle Short Short Short EntityMetadata

  -- | __Spawn Painting:__
  -- This packet shows location, name, and type of painting.
  | CBSpawnPainting VarInt UUID T.Text Position Int8

  -- | __Spawn Player:__
  -- This packet is sent by the server when a player comes into visible range,
  -- not when a player joins.
  --
  -- This packet must be sent after the Player List Item (Play, 0x38, clientbound)
  -- packet that adds the player data for the client to use when spawning a player.
  -- If the Player List Item for the player spawned by this packet is not present
  -- when this packet arrives, Notchian clients will not spawn the player entity.
  -- The Player List Item packet includes skin/cape data.
  --
  -- Servers can, however, safely spawn player entities for players not in
  -- visible range. The client appears to handle it correctly.
  | CBSpawnPlayer VarInt UUID Double Double Double Angle Angle EntityMetadata

  -- | __Animation:__
  -- Sent whenever an entity should change animation.
  | CBAnimation VarInt Animation

  | CBStatistics (V.Vector Statistic)

  -- | __Block Break Animation:__
  -- 0–9 are the displayable destroy stages and each other number means that
  -- there is no animation on this coordinate.
  --
  -- Block break animations can still be applied on air; the animation will
  -- remain visible although there is no block being broken. However,
  -- if this is applied to a transparent block, odd graphical effects may happen,
  -- including water losing its transparency. (An effect similar to this can be
  -- seen in normal gameplay when breaking ice blocks)
  --
  -- If you need to display several break animations at the same time you have
  -- to give each of them a unique Entity ID.
  | CBBlockBreakAnimation VarInt Position Int8

  -- | __Update Block Entity:__
  -- Sets tile entity associated with the block at the given location.
  | CBUpdateBlockEntity Position UpdateBlockEntityAction NBT
  -- | __Block Action:__
  -- This packet is used for a number of things:
  --
  -- * Chests opening and closing
  -- * Pistons pushing and pulling
  -- * Note blocks playing
  -- * Updating beacons
  --
  -- See also: Block Actions
  | CBBlockAction Position BlockAction

  -- | __Block Change:__
  -- Fired whenever a block is changed within the render distance.
  | CBBlockChange Position VarInt

  | CBBossBar UUID BossBarAction

  -- | __Server Difficulty:__
  -- Changes the difficulty setting in the client's option menu.
  | CBServerDifficulty DifficultyField

  -- | __Tab-Complete:__
  -- The server responds with a list of auto-completions of the
  -- last word sent to it.
  -- In the case of regular chat, this is a player username.
  -- Command names and parameters are also supported.
  | CBTabComplete (V.Vector T.Text)

  -- | __Chat Message (clientbound):__
  -- Identifying the difference between Chat/System Message is important as it helps respect the user's chat visibility options. While Position 2 accepts json formatting it will not display, old style formatting works.
  | CBChatMessage Chat Int8

  -- | __Multi Block Change:__
  -- Fired whenever 2 or more blocks are changed within the same chunk on the same tick.
  | CBMultiBlockChange Int32 Int32 (V.Vector BlockChange)

  -- | __Confirm Transaction (clientbound):__
  --  A packet from the server indicating whether a request from the client was accepted,
  --  or whether there was a conflict (due to lag).
  | CBConfirmTransaction Int8 Short Bool

  -- | __Close Window (clientbound):__
  -- This packet is sent from the server to the client when a window is forcibly closed,
  -- such as when a chest is destroyed while it's open.
  --
  -- Note, notchian clients send a close window packet with Window ID 0
  -- to close their inventory even though there is never an Open Window packet for inventory.
  | CBCloseWindow Word8

  -- | __Open Window:__
  -- This is sent to the client when it should open an inventory,
  -- such as a chest, workbench, or furnace.
  -- This message is not sent anywhere for clients opening their own inventory.
  | CBOpenWindow Word8 (Either Int32 T.Text) Chat Word8

  -- | __Window Items:__
  -- Sent by the server when items in multiple slots (in a window) are added/removed.
  -- This includes the main inventory, equipped armour and crafting slots.
  | CBWindowItems Word8 (V.Vector Slot)

  -- | __Window Property:__
  -- This packet is used to inform the client that part of a GUI window should be updated.
  | CBWindowProperty Word8 Short Short

  -- | __Set Slot:__
  -- Sent by the server when an item in a slot (in a window) is added/removed.
  | CBSetSlot Int8 Short Slot

  -- | __Set Cooldown:__
  -- Applies a cooldown period to all items with the given type. Used by the Notchian server with enderpearls. This packet should be sent when the cooldown starts and also when the cooldown ends (to compensate for lag), although the client will end the cooldown automatically.
  | CBSetCooldown VarInt VarInt

  -- | __Plugin Message (clientbound):__
  -- Mods and plugins can use this to send their data. Minecraft itself uses a number of plugin channels. These internal channels are prefixed with @MC|@.
  | CBPluginMessage T.Text B.ByteString

  -- | __Named Sound Effect:__
  -- Used to play a sound effect on the client. Custom sounds may be added by resource packs.
  | CBNamedSoundEffect T.Text VarInt Int32 Int32 Int32 Float Float

  -- | __Disconnect (play):__
  -- Sent by the server before it disconnects a client. The client assumes that the server has already closed the connection by the time the packet arrives.
  | CBPlayDisconnect Chat

  | CBEntityStatus Int32 EntityStatus

  -- | __Explosion:__
  -- Sent when an explosion occurs (creepers, TNT, and ghast fireballs).
  --
  -- Each block in Records is set to air. Coordinates for each axis in record is int(X) + record.x
  | CBExplosion Float Float Float Float (V.Vector (Int8,Int8,Int8)) Float Float Float

  -- | __Unload Chunk:__
  -- Tells the client to unload a chunk column.
  | CBUnloadChunk Int32 Int32

  -- | __Change Game State:__
  -- Used for a wide variety of game state things, from weather to bed use to game mode to demo messages.
  | CBChangeGameState GameChangeReason Float

  -- | __Keep Alive (clientbound):__
  -- The server will frequently send out a keep-alive, each containing a random ID. The client must respond with the same packet. If the client does not respond to them for over 30 seconds, the server kicks the client. Vice versa, if the server does not send any keep-alives for 20 seconds, the client will disconnect and yields a "Timed out" exception.
  | CBKeepAlive VarInt

  -- | __Chunk Data:__
  -- The server only sends skylight information for chunk pillars in the Overworld, it's up to the client to know in which dimenison the player is currently located. You can also infer this information from the primary bitmask and the amount of uncompressed bytes sent. This packet also sends all block entities in the chunk (though sending them is not required; it is still legal to send them with Update Block Entity later).
  | CBChunkData Int32 Int32 Bool Int (V.Vector ChunkSection) (Maybe B.ByteString) (V.Vector NBT)

  -- | __Effect:__
  -- Sent when a client is to play a sound or particle effect.
  --
  -- By default, the Minecraft client adjusts the volume of sound effects based on distance. The final boolean field is used to disable this, and instead the effect is played from 2 blocks away in the correct direction. Currently this is only used for effect 1023 (wither spawn) and effect 1028 (enderdragon death); it is ignored on other effects.
  | CBEffect Int32 Position Int32 Bool

  -- | __Particle:__
  -- Displays the named particle.
  | CBParticle Int32 Bool Float Float Float Float Float Float Float Int32 (V.Vector VarInt)

  -- | __Join Game:__
  -- See Protocol Encryption for information on logging in.
  | CBJoinGame Int32 Word8 Int32 Word8 Word8 T.Text Bool

  -- | __Map:__
  -- Updates a rectangular area on a map item.
  | CBMap VarInt Int8 Bool (V.Vector Icon) UpdatedColumns

  -- | __Entity Relative Move:__
  -- This packet is sent by the server when an entity moves less then 8 blocks; if an entity moves more than 8 blocks Entity Teleport (Play, 0x4A, clientbound) should be sent instead.
  --
  -- This packet allows at most 8 blocks movement in any direction, because short range is from -32768 to 32767. And 32768 / (128 * 32) = 8.
  | CBEntityRelativeMove VarInt Short Short Short Bool

  -- | __Entity Look And Relative Move:__
  -- This packet is sent by the server when an entity rotates and moves. Since a short range is limited from -32768 to 32767, and movement is offset of fixed-point numbers, this packet allows at most 8 blocks movement in any direction. (-32768 / (32 * 128) == -8)
  | CBEntityLookAndRelativeMove VarInt Short Short Short Angle Angle Bool

  -- | __Entity Look:__
  -- This packet is sent by the server when an entity rotates.
  | CBEntityLook VarInt Angle Angle Bool

  -- | __Entity:__
  -- This packet may be used to initialize an entity.
  --
  -- For player entities, either this packet or any move/look packet is sent every game tick. So the meaning of this packet is basically that the entity did not move/look since the last such packet.
  | CBEntity VarInt

  -- | __Vehicle Move (clientbound):__
  -- Note that all fields use absolute positioning and do not allow for relative positioning.
  | CBVehicleMove Double Double Double Float Float

  -- | __Open Sign Editor:__
  -- Sent when the client has placed a sign and is allowed to send Update Sign.
  | CBOpenSignEditor Position

  -- | __Player Abilities (clientbound):__
  -- The latter 2 floats are used to indicate the field of view and flying speed respectively, while the first byte is used to determine the value of 4 booleans.
  | CBPlayerAbilities Int8 Float Float

  | CBCombatEvent CombatEvent

  -- | __Player List Item:__
  -- Sent by the server to update the user list (<tab> in the client).
  | CBPlayerListItem Int (V.Vector PlayerListEntry)

  -- | __Player Position And Look (clientbound):__
  -- Updates the player's position on the server. This packet will also close the “Downloading Terrain” screen when joining/respawning.
  --
  -- If the distance between the last known position of the player on the server and the new position set by this packet is greater than 100 meters, the client will be kicked for “You moved too quickly :( (Hacking?)”.
  --
  -- Also if the fixed-point number of X or Z is set greater than 3.2E7D the client will be kicked for “Illegal position”.
  --
  -- Yaw is measured in degrees, and does not follow classical trigonometry rules. The unit circle of yaw on the XZ-plane starts at (0, 1) and turns counterclockwise, with 90 at (-1, 0), 180 at (0, -1) and 270 at (1, 0). Additionally, yaw is not clamped to between 0 and 360 degrees; any number is valid, including negative numbers and numbers greater than 360.
  --
  -- Pitch is measured in degrees, where 0 is looking straight ahead, -90 is looking straight up, and 90 is looking straight down.
  | CBPlayerPositionAndLook Double Double Double Float Float Int8 VarInt

  -- | __Use Bed:__
  -- This packet tells that a player goes to bed.
  --
  -- The client with the matching Entity ID will go into bed mode.
  --
  -- This Packet is sent to all nearby players including the one sent to bed.
  | CBUseBed VarInt Position

  -- | __Destroy Entities:__
  -- Sent by the server when a list of entities is to be destroyed on the client.
  | CBDestroyEntities (V.Vector VarInt)

  | CBRemoveEntityEffect VarInt Int8
  | CBResourcePackSend T.Text T.Text

  -- | __Respawn:__
  -- To change the player's dimension (overworld/nether/end), send them a respawn packet with the appropriate dimension, followed by prechunks/chunks for the new dimension, and finally a position and look packet. You do not need to unload chunks, the client will do it automatically.
  | CBRespawn DimensionField DifficultyField GameModeField T.Text

  -- | __Entity Head Look:__
  -- Changes the direction an entity's head is facing.
  | CBEntityHeadLook VarInt Angle

  | CBWorldBorder WorldBorderAction

  -- | __Camera:__
  -- Sets the entity that the player renders from. This is normally used when the player left-clicks an entity while in spectator mode.
  --
  -- The player's camera will move with the entity and look where it is looking. The entity is often another player, but can be any type of entity. The player is unable to move this entity (move packets will act as if they are coming from the other entity).
  --
  -- If the given entity is not loaded by the player, this packet is ignored. To return control to the player, send this packet with their entity ID.
  --
  -- The Notchian server resets this (sends it back to the default entity) whenever the spectated entity is killed or the player sneaks, but only if they were spectating an entity. It also sends this packet whenever the player switches out of spectator mode (even if they weren't spectating an entity).
  | CBCamera VarInt

  -- | __Held Item Change (clientbound):__
  -- Sent to change the player's slot selection.
  | CBHeldItemChange Int8

  -- | __Display Scoreboard:__
  -- This is sent to the client when it should display a scoreboard.
  | CBDisplayScoreboard Int8 T.Text

  -- | __Entity Metadata:__
  -- Updates one or more metadata properties for an existing entity. Any properties not included in the Metadata field are left unchanged.
  | CBEntityMetadata VarInt EntityMetadata

  -- | __Attach Entity:__
  -- This packet is sent when an entity has been leashed to another entity.
  | CBAttachEntity Int32 Int32

  -- | __Entity Velocity:__
  -- Velocity is believed to be in units of 1/8000 of a block per server tick (50ms); for example, -1343 would move (-1343 / 8000) = −0.167875 blocks per tick (or −3,3575 blocks per second).
  | CBEntityVelocity VarInt Short Short Short

  | CBEntityEquipment VarInt VarInt Slot

  -- | __Set Experience:__
  -- Sent by the server when the client should change experience levels.
  | CBSetExperience Float VarInt VarInt

  -- | __Update Health:__
  -- Sent by the server to update/set the health of the player it is sent to.
  --
  -- Food saturation acts as a food “overcharge”. Food values will not decrease while the saturation is over zero. Players logging in automatically get a saturation of 5.0. Eating food increases the saturation as well as the food bar.
  | CBUpdateHealth Float VarInt Float

  -- | __Scoreboard Objective:__
  -- This is sent to the client when it should create a new scoreboard objective or remove one.
  | CBScoreboardObjective T.Text ScoreboardMode

  | CBSetPassengers VarInt (V.Vector VarInt)

  -- | __Teams:__
  -- Creates and updates teams.
  | CBTeams T.Text TeamMode

  -- | __Update Score:__
  -- This is sent to the client when it should update a scoreboard item.
  | CBUpdateScore UpdateScoreAction

  -- | __Spawn Position:__
  -- Sent by the server after login to specify the coordinates of the spawn point (the point at which players spawn at, and which the compass points to). It can be sent at any time to update the point compasses point at.
  | CBSpawnPosition Position

  -- | __Time Update:__
  -- Time is based on ticks, where 20 ticks happen every second. There are 24000 ticks in a day, making Minecraft days exactly 20 minutes long.
  --
  -- The time of day is based on the timestamp modulo 24000. 0 is sunrise, 6000 is noon, 12000 is sunset, and 18000 is midnight.
  --
  -- The default SMP server increments the time by 20 every second.
  | CBTimeUpdate Int64 Int64

  | CBTitle TitleAction

  -- | __Sound Effect:__
  -- This packet is used to play a number of hardcoded sound events. For custom sounds, use Named Sound Effect (Play, 0x19, clientbound).
  | CBSoundEffect VarInt VarInt Int32 Int32 Int32 Float Float

  -- | __Player List Header And Footer:__
  -- This packet may be used by custom servers to display additional information above/below the player list. It is never sent by the Notchian server.
  | CBPlayerListHeaderAndFooter Chat Chat

  -- | __Collect Item:__
  -- Sent by the server when someone picks up an item lying on the ground — its sole purpose appears to be the animation of the item flying towards you. It doesn't destroy the entity in the client memory, and it doesn't add it to your inventory. The server only checks for items to be picked up after each Player Position (and Player Position And Look) packet sent by the client.
  | CBCollectItem VarInt VarInt

  -- | __Entity Teleport:__
  -- This packet is sent by the server when an entity moves more than 4 blocks.
  | CBEntityTeleport VarInt Double Double Double Angle Angle Bool

  -- | __Entity Properties:__
  -- Sets attributes on the given entity.
  | CBEntityProperties VarInt (V.Vector EntityProperty)
  | CBEntityEffect VarInt Int8 Int8 VarInt Word8
  deriving (Show,Eq)


encodeCBPlay :: CBPlay -> Encode.Builder
encodeCBPlay (CBSpawnObject entityID objectUUID entityType x y z pitch yaw dat vX vY vZ) =
  Encode.word8 0x00
  <> encodeVarInt entityID
  <> encodeUUID objectUUID
  <> Encode.int8 entityType
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z
  <> encodeAngle pitch
  <> encodeAngle yaw
  <> Encode.int32BE dat
  <> Encode.int16BE vX
  <> Encode.int16BE vY
  <> Encode.int16BE vZ

encodeCBPlay (CBSpawnExperienceOrb entityID x y z count) =
  Encode.word8 0x01
  <> encodeVarInt entityID
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z
  <> Encode.int16BE count

encodeCBPlay (CBSpawnGlobalEntity entityID entityType x y z) =
  Encode.word8 0x02
  <> encodeVarInt entityID
  <> Encode.int8 entityType
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z

encodeCBPlay (CBSpawnMob entityID entityUUID entityType x y z yaw pitch headPitch vX vY vZ metadata) =
  Encode.word8 0x03
  <> encodeVarInt entityID
  <> encodeUUID entityUUID
  <> Encode.word8 entityType
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z
  <> encodeAngle yaw
  <> encodeAngle pitch
  <> encodeAngle headPitch
  <> Encode.int16BE vX
  <> Encode.int16BE vY
  <> Encode.int16BE vZ
  <> encodeEntityMetadata metadata

encodeCBPlay (CBSpawnPainting entityID entityUUID title location direction) =
  Encode.word8 0x04
  <> encodeVarInt entityID
  <> encodeUUID entityUUID
  <> encodeText title
  <> encodePosition location
  <> Encode.int8 direction

encodeCBPlay (CBSpawnPlayer entityID playerUUID x y z yaw pitch metadata) =
  Encode.word8 0x05
  <> encodeVarInt entityID
  <> encodeUUID playerUUID
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z
  <> encodeAngle yaw
  <> encodeAngle pitch
  <> encodeEntityMetadata metadata

encodeCBPlay (CBAnimation entityID animation) =
  Encode.word8 0x06
  <> (encodeVarInt entityID)
  <> (Encode.word8 . toEnum . fromEnum $ animation)

encodeCBPlay (CBStatistics statistics) =
  Encode.word8 0x07
  <> (encodeVarInt . V.length $ statistics)
  <> (V.foldl' (<>) mempty (fmap encodeStatistic statistics))

encodeCBPlay (CBBlockBreakAnimation entityID location destroyStage) =
  Encode.word8 0x08
  <> encodeVarInt entityID
  <> encodePosition location
  <> Encode.int8 destroyStage

encodeCBPlay (CBUpdateBlockEntity location action nbtData) =
  Encode.word8 0x09
  <> encodePosition location
  <> (Encode.word8 . toEnum . fromEnum $ action)
  <> encodeNBT nbtData

encodeCBPlay (CBBlockAction location blockAction) =
  Encode.word8 0x0A
  <> encodePosition location
  <> case blockAction of
      NoteBlockAction instrumentType notePitch  -> do
        (Encode.word8 . toEnum . fromEnum $ instrumentType)
        <> (Encode.word8 . toEnum . fromEnum $ notePitch)
        <> encodeVarInt 25
      PistonBlockAction pistonState pistonDirection -> do
        (Encode.word8 . toEnum . fromEnum $ pistonState)
        <> (Encode.word8 . toEnum . fromEnum $ pistonDirection)
        <> encodeVarInt 33
      ChestBlockAction byte -> do
        Encode.word8 1
        <> Encode.word8 byte
        <> encodeVarInt 54

encodeCBPlay (CBBlockChange location blockID) =
  Encode.word8 0x0B
  <> encodePosition location
  <> encodeVarInt blockID

encodeCBPlay (CBBossBar uuid bossBarAction) =
  Encode.word8 0x0C
  <> encodeUUID uuid
  <> case bossBarAction of
      BossBarAdd title health color division flags -> do
        encodeVarInt 0
        <> encodeText title
        <> Encode.floatBE health
        <> encodeVarInt color
        <> encodeVarInt division
        <> Encode.word8 flags

      BossBarRemove -> do
        encodeVarInt 1

      BossBarUpdateHealth health -> do
        encodeVarInt 2
        <> Encode.floatBE health

      BossBarUpdateTitle title -> do
        encodeVarInt 3
        <> encodeText title

      BossBarUpdateStyle color dividers -> do
        encodeVarInt 4
        <> (encodeVarInt . toEnum $ color)
        <> (encodeVarInt . toEnum $ dividers)

      BossBarUpdateFlags flags -> do
        encodeVarInt 5
        <> Encode.word8 flags

encodeCBPlay (CBServerDifficulty difficulty) =
  Encode.word8 0x0D
  <> (Encode.word8 . toEnum . fromEnum $ difficulty)

encodeCBPlay (CBTabComplete matches) =
  Encode.word8 0x0E
  <> (encodeVarInt . V.length $ matches)
  <> V.foldl' (<>) mempty (fmap encodeText matches)

encodeCBPlay (CBChatMessage jsonData position) =
  Encode.word8 0x0F
  <> encodeText jsonData
  <> Encode.int8 position

encodeCBPlay (CBMultiBlockChange chunkX chunkZ records) =
  Encode.word8 0x10
  <> Encode.int32BE chunkX
  <> Encode.int32BE chunkZ
  <> (encodeVarInt . V.length $ records)
  <> V.foldl' (<>) mempty (fmap encodeRecord records)

encodeCBPlay (CBConfirmTransaction windowID actionNumber accepted) =
  Encode.word8 0x11
  <> Encode.int8 windowID
  <> Encode.int16BE actionNumber
  <> (Encode.word8 . toEnum . fromEnum $ accepted)

encodeCBPlay (CBCloseWindow windowID) =
  Encode.word8 0x12
  <> Encode.word8 windowID

encodeCBPlay (CBOpenWindow windowID windowType windowTitle numOfSlots) =
  Encode.word8 0x13
  <> Encode.word8 windowID
  <> case windowType of
        Left entityID -> do
          encodeText "EntityHorse"
          <> encodeText windowTitle
          <> Encode.word8 numOfSlots
          <> Encode.int32BE entityID
        Right windowType' -> do
          encodeText windowType'
          <> encodeText windowTitle
          <> Encode.word8 numOfSlots

encodeCBPlay (CBWindowItems windowID slotData) =
  Encode.word8 0x14
  <> Encode.word8 windowID
  <> (Encode.int16BE . toEnum . V.length $ slotData)
  <> V.foldl' (<>) mempty (fmap encodeSlot slotData)

encodeCBPlay (CBWindowProperty windowID property value) =
  Encode.word8 0x15
  <> Encode.word8 windowID
  <> Encode.int16BE property -- Should move to WindowProperty
  <> Encode.int16BE value

encodeCBPlay (CBSetSlot windowID slot slotData) =
  Encode.word8 0x16
  <> Encode.int8 windowID
  <> Encode.int16BE slot
  <> encodeSlot slotData

encodeCBPlay (CBSetCooldown itemID cooldownTicks) =
  Encode.word8 0x17
  <> encodeVarInt itemID
  <> encodeVarInt cooldownTicks

encodeCBPlay (CBPluginMessage channel dat) =
  Encode.word8 0x18
  <> encodeText channel
  <> Encode.byteString dat

encodeCBPlay (CBNamedSoundEffect soundName soundCategory effPosX effPosY effPosZ volume pitch) =
  Encode.word8 0x19
  <> encodeText soundName
  <> encodeVarInt soundCategory
  <> Encode.int32BE effPosX
  <> Encode.int32BE effPosY
  <> Encode.int32BE effPosZ
  <> Encode.floatBE volume
  <> Encode.floatBE pitch

encodeCBPlay (CBPlayDisconnect reason) =
  Encode.word8 0x1A
  <> encodeText reason

encodeCBPlay (CBEntityStatus entityID entityStatus) =
  Encode.word8 0x1B
  <> Encode.int32BE entityID
  <> (Encode.int8 . toEnum . fromEnum $ entityStatus)

encodeCBPlay (CBExplosion x y z radius records pMotionX pMotionY pMotionZ) =
  Encode.word8 0x1C
  <> Encode.floatBE x
  <> Encode.floatBE y
  <> Encode.floatBE z
  <> Encode.floatBE radius
  <> (Encode.int32BE . toEnum . V.length $ records)
  <> V.foldl' (<>) mempty (fmap (\(a,b,c) -> Encode.int8 a <> Encode.int8 b <> Encode.int8 c) records)
  <> Encode.floatBE pMotionX
  <> Encode.floatBE pMotionY
  <> Encode.floatBE pMotionZ

encodeCBPlay (CBUnloadChunk chunkX chunkZ) =
  Encode.word8 0x1D
  <> Encode.int32BE chunkX
  <> Encode.int32BE chunkZ

encodeCBPlay (CBChangeGameState reason value) =
  Encode.word8 0x1E
  <> (Encode.word8 . toEnum . fromEnum $ reason)
  <> Encode.floatBE value

encodeCBPlay (CBKeepAlive keepAliveID) =
  Encode.word8 0x1F
  <> encodeVarInt keepAliveID

encodeCBPlay (CBChunkData chunkX chunkZ groundUpCont primaryBitMask dat biomes blockEntities) =
  Encode.word8 0x20
  <> Encode.int32BE chunkX
  <> Encode.int32BE chunkZ
  <> (Encode.word8 . toEnum . fromEnum $ groundUpCont)
  <> encodeVarInt primaryBitMask
  <> (encodeVarInt . V.length $ dat)
  <> V.foldl' (<>) mempty (fmap (encodeChunkSection primaryBitMask) dat)
  <> case (groundUpCont,biomes) of
      (True,Just b)   -> Encode.byteString b
      _               -> mempty

  <> (encodeVarInt . V.length $ blockEntities)
  <> V.foldl' (<>) mempty (fmap encodeNBT blockEntities)

-- (NOTE) Should be better typed to Effect IDs that actually exist
encodeCBPlay (CBEffect effectID location dat disableRelativeVolume) =
  Encode.word8 0x21
  <> Encode.int32BE effectID
  <> encodePosition location
  <> Encode.int32BE dat
  <> (Encode.word8 . toEnum . fromEnum $ disableRelativeVolume)

encodeCBPlay (CBParticle particleID longDistance x y z offsetX offsetY offsetZ particleData particleCount dat) =
  Encode.word8 0x22
  <> Encode.int32BE particleID
  <> (Encode.word8 . toEnum . fromEnum $ longDistance)
  <> Encode.floatBE x
  <> Encode.floatBE y
  <> Encode.floatBE z
  <> Encode.floatBE offsetX
  <> Encode.floatBE offsetY
  <> Encode.floatBE offsetZ
  <> Encode.floatBE particleData
  <> Encode.int32BE particleCount
  <> V.foldl' (<>) mempty (fmap encodeVarInt dat)

encodeCBPlay (CBJoinGame entityID gameMode dimension difficulty maxPlayers levelType reduceDebug) =
  Encode.word8 0x23
  <> Encode.int32BE entityID
  <> Encode.word8 gameMode
  <> Encode.int32BE dimension
  <> Encode.word8 difficulty
  <> Encode.word8 maxPlayers
  <> encodeText levelType
  <> (Encode.word8 . toEnum . fromEnum $ reduceDebug)

encodeCBPlay (CBMap itemDamage scale trackingPosition icons updatedColumns) =
  Encode.word8 0x24
  <> encodeVarInt itemDamage
  <> Encode.int8 scale
  <> (Encode.word8 . toEnum . fromEnum $ trackingPosition)
  <> (encodeVarInt . V.length $ icons)
  <> V.foldl' (<>) mempty (fmap encodeIcon icons)
  <> case updatedColumns of
      NoUpdatedColumns -> do
        Encode.int8 0
      (UpdatedColumns col rows x z dat) -> do
        Encode.int8 col
        <> Encode.int8 rows
        <> Encode.int8 x
        <> Encode.int8 z
        <> (encodeVarInt . B.length $ dat)
        <> Encode.byteString dat

encodeCBPlay (CBEntityRelativeMove entityID dX dY dZ onGround) =
  Encode.word8 0x25
  <> encodeVarInt entityID
  <> Encode.int16BE dX
  <> Encode.int16BE dY
  <> Encode.int16BE dZ
  <> (Encode.word8 . toEnum . fromEnum $ onGround)

encodeCBPlay (CBEntityLookAndRelativeMove entityID dX dY dZ yaw pitch onGround) =
  Encode.word8 0x26
  <> encodeVarInt entityID
  <> Encode.int16BE dX
  <> Encode.int16BE dY
  <> Encode.int16BE dZ
  <> encodeAngle yaw
  <> encodeAngle pitch
  <> (Encode.word8 . toEnum . fromEnum $ onGround)

encodeCBPlay (CBEntityLook entityID yaw pitch onGround) =
  Encode.word8 0x27
  <> encodeVarInt entityID
  <> encodeAngle yaw
  <> encodeAngle pitch
  <> (Encode.word8 . toEnum . fromEnum $ onGround)

encodeCBPlay (CBEntity entityID) =
  Encode.word8 0x28
  <> encodeVarInt entityID

encodeCBPlay (CBVehicleMove x y z yaw pitch) =
  Encode.word8 0x29
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z
  <> Encode.floatBE yaw
  <> Encode.floatBE pitch

encodeCBPlay (CBOpenSignEditor location) =
  Encode.word8 0x2A
  <> encodePosition location

-- flags should be better typed
encodeCBPlay (CBPlayerAbilities flags flyingSpeed viewModifiers) =
  Encode.word8 0x2B
  <> Encode.int8 flags
  <> Encode.floatBE flyingSpeed
  <> Encode.floatBE viewModifiers

encodeCBPlay (CBCombatEvent combatEvent) =
  Encode.word8 0x2C
  <> case combatEvent of
      EnterCombat   -> do
        encodeVarInt 0
      EndCombat duration entityID ->  do
        encodeVarInt 1
        <> encodeVarInt duration
        <> Encode.int32BE entityID
      EntityDead playerID entityID message -> do
        encodeVarInt 2
        <> encodeVarInt playerID
        <> Encode.int32BE entityID
        <> encodeText message

encodeCBPlay (CBPlayerListItem action players) =
  Encode.word8 0x2D
  <> encodeVarInt action
  <> (encodeVarInt . V.length $ players)
  <> V.foldl' (<>) mempty (fmap (encodePlayerListEntry action) players)

-- flags should be better typed
encodeCBPlay (CBPlayerPositionAndLook x y z yaw pitch flags teleportID) =
  Encode.word8 0x2E
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z
  <> Encode.floatBE yaw
  <> Encode.floatBE pitch
  <> Encode.int8 flags
  <> encodeVarInt teleportID

encodeCBPlay (CBUseBed entityID location) =
  Encode.word8 0x2F
  <> encodeVarInt entityID
  <> encodePosition location

encodeCBPlay (CBDestroyEntities entityIDs) =
  Encode.word8 0x30
  <> (encodeVarInt . V.length $ entityIDs)
  <> V.foldl' (<>) mempty (fmap encodeVarInt entityIDs)

encodeCBPlay (CBRemoveEntityEffect entityID effectID) =
  Encode.word8 0x31
  <> encodeVarInt entityID
  <> Encode.int8 effectID

encodeCBPlay (CBResourcePackSend url hash) =
  Encode.word8 0x32
  <> encodeText url
  <> encodeText hash

encodeCBPlay (CBRespawn dimension difficulty gameMode levelType) =
  Encode.word8 0x33
  <> (Encode.int32BE . toEnum . fromEnum $ dimension)
  <> (Encode.word8 . toEnum . fromEnum $ difficulty)
  <> (Encode.word8 . toEnum . fromEnum $ gameMode)
  <> encodeText levelType

encodeCBPlay (CBEntityHeadLook entityID headYaw) =
  Encode.word8 0x34
  <> encodeVarInt entityID
  <> encodeAngle headYaw

encodeCBPlay (CBWorldBorder worldBorderAction) =
  Encode.word8 0x35
  <> case worldBorderAction of
      SetSize diameter -> do
        encodeVarInt 0
        <> Encode.doubleBE diameter
      LerpSize oldDiameter newDiameter speed -> do
        encodeVarInt 1
        <> Encode.doubleBE oldDiameter
        <> Encode.doubleBE newDiameter
        <> encodeVarLong speed
      SetCenter x z -> do
        encodeVarInt 2
        <> Encode.doubleBE x
        <> Encode.doubleBE z
      Initialize x z oldDiameter newDiameter speed portalBoundary warningTime warningBlocks -> do
        encodeVarInt 3
        <> Encode.doubleBE x
        <> Encode.doubleBE z
        <> Encode.doubleBE oldDiameter
        <> Encode.doubleBE newDiameter
        <> encodeVarLong speed
        <> encodeVarInt portalBoundary
        <> encodeVarInt warningTime
        <> encodeVarInt warningBlocks
      SetWarningTime warningTime -> do
        encodeVarInt 4
        <> (encodeVarInt . toEnum $ warningTime)
      SetWarningBlocks warningBlocks -> do
        encodeVarInt 5
        <> (encodeVarInt . toEnum $ warningBlocks)

encodeCBPlay (CBCamera cameraID) =
  Encode.word8 0x36
  <> encodeVarInt cameraID

encodeCBPlay (CBHeldItemChange slot) =
  Encode.word8 0x37
  <> Encode.int8 slot

encodeCBPlay (CBDisplayScoreboard position scoreName) =
  Encode.word8 0x38
  <> Encode.int8 position
  <> encodeText scoreName

encodeCBPlay (CBEntityMetadata entityID metadata) =
  Encode.word8 0x39
  <> encodeVarInt entityID
  <> encodeEntityMetadata metadata

encodeCBPlay (CBAttachEntity attachedEntityID holdingEntityID) =
  Encode.word8 0x3A
  <> Encode.int32BE attachedEntityID
  <> Encode.int32BE holdingEntityID

encodeCBPlay (CBEntityVelocity entityID vX vY vZ) =
  Encode.word8 0x3B
  <> encodeVarInt entityID
  <> Encode.int16BE vX
  <> Encode.int16BE vY
  <> Encode.int16BE vZ

encodeCBPlay (CBEntityEquipment entityID slot item) =
  Encode.word8 0x3C
  <> encodeVarInt entityID
  <> encodeVarInt slot
  <> encodeSlot item

encodeCBPlay (CBSetExperience experienceBar level totalExperience) =
  Encode.word8 0x3D
  <> Encode.floatBE experienceBar
  <> encodeVarInt level
  <> encodeVarInt totalExperience

encodeCBPlay (CBUpdateHealth health food foodSaturation) =
  Encode.word8 0x3E
  <> Encode.floatBE health
  <> encodeVarInt food
  <> Encode.floatBE foodSaturation

encodeCBPlay (CBScoreboardObjective objectiveName mode) =
  Encode.word8 0x3F
  <> encodeText objectiveName
  <> Encode.int8 (encodeScoreboardMode mode)
  <> case mode of
      CreateScoreboard ov t -> do
        encodeText ov
        <> encodeText t
      RemoveScoreboard -> do
        mempty
      UpdateDisplayText ov t -> do
        encodeText ov
        <> encodeText t

encodeCBPlay (CBSetPassengers entityID passengers) =
  Encode.word8 0x40
  <> encodeVarInt entityID
  <> (encodeVarInt . V.length $ passengers)
  <> V.foldl' (<>) mempty (fmap encodeVarInt passengers)

encodeCBPlay (CBTeams teamName mode) =
  Encode.word8 0x41
  <> encodeText teamName
  <> case mode of
      CreateTeam displayName prefix suffix flags tagVisibility collision color players -> do
        Encode.int8 0
        <> encodeText displayName
        <> encodeText prefix
        <> encodeText suffix
        <> Encode.int8 flags
        <> encodeText tagVisibility
        <> encodeText collision
        <> Encode.int8 color
        <> (encodeVarInt . V.length $ players)
        <> V.foldl' (<>) mempty (fmap encodeText players)
      RemoveTeam -> do
        Encode.int8 1
      UpdateTeamInfo displayName prefix suffix flags tagVisibility collision color -> do
        Encode.int8 2
        <> encodeText displayName
        <> encodeText prefix
        <> encodeText suffix
        <> Encode.int8 flags
        <> encodeText tagVisibility
        <> encodeText collision
        <> Encode.int8 color
      AddPlayers players -> do
        Encode.int8 3
        <> (encodeVarInt . V.length $ players)
        <> V.foldl' (<>) mempty (fmap encodeText players)
      RemovePlayers players -> do
        Encode.int8 4
        <> (encodeVarInt . V.length $ players)
        <> V.foldl' (<>) mempty (fmap encodeText players)

encodeCBPlay (CBUpdateScore action) =
  Encode.word8 0x42
  <> case action of
      (CreateOrUpdateScoreItem scoreName objectiveName val) -> do
        encodeText scoreName
        <> Encode.int8 0
        <> encodeText objectiveName
        <> encodeVarInt val
      (RemoveScoreItem scoreName objectiveName) -> do
        encodeText scoreName
        <> Encode.int8 1
        <> encodeText objectiveName

encodeCBPlay (CBSpawnPosition location) =
  Encode.word8 0x43
  <> encodePosition location

encodeCBPlay (CBTimeUpdate worldAge timeOfDay) =
  Encode.word8 0x44
  <> Encode.int64BE worldAge
  <> Encode.int64BE timeOfDay

encodeCBPlay (CBTitle titleAction) =
  Encode.word8 0x45
  <> case titleAction of
      SetTitle titleText -> do
        encodeVarInt 0
        <> encodeText titleText
      SetSubtitle subtitleText -> do
        encodeVarInt 1
        <> encodeText subtitleText
      SetTimesAndDisplay fadeIn stay fadeOut -> do
        encodeVarInt 2
        <> Encode.int32BE fadeIn
        <> Encode.int32BE stay
        <> Encode.int32BE fadeOut
      Hide -> do
        encodeVarInt 3
      Reset -> do
        encodeVarInt 4

encodeCBPlay (CBSoundEffect soundID soundCategory effPosX effPosY effPosZ volume pitch) =
  Encode.word8 0x46
  <> encodeVarInt soundID
  <> encodeVarInt soundCategory
  <> Encode.int32BE effPosX
  <> Encode.int32BE effPosY
  <> Encode.int32BE effPosZ
  <> Encode.floatBE volume
  <> Encode.floatBE pitch

encodeCBPlay (CBPlayerListHeaderAndFooter header footer) =
  Encode.word8 0x47
  <> encodeText header
  <> encodeText footer

encodeCBPlay (CBCollectItem collectedEntityID collectorEntityID) =
  Encode.word8 0x48
  <> encodeVarInt collectedEntityID
  <> encodeVarInt collectorEntityID

encodeCBPlay (CBEntityTeleport entityID x y z yaw pitch onGround) =
  Encode.word8 0x49
  <> encodeVarInt entityID
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z
  <> encodeAngle yaw
  <> encodeAngle pitch
  <> (Encode.word8 . toEnum . fromEnum $ onGround)

-- Needs to be better typed
encodeCBPlay (CBEntityProperties entityID properties) =
  Encode.word8 0x4A
  <> encodeVarInt entityID
  <> (Encode.int32BE . toEnum . V.length $ properties)
  <> V.foldl' (<>) mempty (fmap encodeEntityProperty properties)

encodeCBPlay (CBEntityEffect entityID effectID amplifier duration hideParticles) =
  Encode.word8 0x4B
  <> encodeVarInt entityID
  <> Encode.int8 effectID
  <> Encode.int8 amplifier
  <> encodeVarInt duration
  <> (Encode.word8 . toEnum . fromEnum $ hideParticles)

decodeCBPlay :: Decode.Parser CBPlay
decodeCBPlay = do
  packetID <- Decode.anyWord8
  case packetID of
    0x00  -> do
      entityID <- decodeVarInt
      objectUUID <- decodeUUID
      t <- decodeInt8
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      pitch <- decodeAngle
      yaw <- decodeAngle
      dat <- decodeInt32BE
      vX <- decodeInt16BE
      vY <- decodeInt16BE
      vZ <- decodeInt16BE
      return $ CBSpawnObject entityID objectUUID t x y z pitch yaw dat vX vY vZ

    0x01  -> do
      entityID <- decodeVarInt
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      count <- decodeInt16BE
      return $ CBSpawnExperienceOrb entityID x y z count

    0x02  -> do
      entityID <- decodeVarInt
      t <- decodeInt8
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      return $ CBSpawnGlobalEntity entityID t x y z

    0x03  -> do
      entityID <- decodeVarInt
      entityUUID <- decodeUUID
      t <- Decode.anyWord8
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      yaw <- decodeAngle
      pitch <- decodeAngle
      headPitch <- decodeAngle
      vX <- decodeInt16BE
      vY <- decodeInt16BE
      vZ <- decodeInt16BE
      metadata <- decodeEntityMetadata
      return $ CBSpawnMob entityID entityUUID t x y z yaw pitch headPitch vX vY vZ metadata

    0x04  -> do
      entityID <- decodeVarInt
      entityUUID <- decodeUUID
      title <- decodeText
      location <- decodePosition
      direction <- decodeInt8
      return $ CBSpawnPainting entityID entityUUID title location direction

    0x05  -> do
      entityID <- decodeVarInt
      playerUUID <- decodeUUID
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      yaw <- decodeAngle
      pitch <- decodeAngle
      metadata <- decodeEntityMetadata
      return $ CBSpawnPlayer entityID playerUUID x y z yaw pitch metadata

    0x06  -> do
      entityID <- decodeVarInt
      animation <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBAnimation entityID animation

    0x07  -> do
      count <- fmap fromEnum decodeVarInt
      statistics <- (fmap V.fromList $ Decode.count count decodeStatistic)
      return $ CBStatistics statistics

    0x08  -> do
      entityID <- decodeVarInt
      location <- decodePosition
      destroyStage <- decodeInt8
      return $ CBBlockBreakAnimation entityID location destroyStage

    0x09  -> do
      location <- decodePosition
      action <- (fmap (toEnum . fromEnum) Decode.anyWord8)
      nbtData <- decodeNBT
      return $ CBUpdateBlockEntity location action nbtData

    0x0A -> do
      location <- decodePosition
      byte1 <- Decode.anyWord8
      byte2 <- Decode.anyWord8
      blockType <- decodeVarInt
      case blockType of
        25 -> do
          return $ CBBlockAction
            location
            (NoteBlockAction (toEnum . fromEnum $ byte1) (toEnum . fromEnum $ byte2))
        33 -> do
          return $ CBBlockAction
            location
            (PistonBlockAction (toEnum . fromEnum $ byte1) (toEnum . fromEnum $ byte2))
        54 -> do
            return $ CBBlockAction location (ChestBlockAction byte2)
        err -> fail $ "Error: invalid BlockAction type: " ++ show err

    0x0B -> do
      location <- decodePosition
      blockID <- decodeVarInt
      return $ CBBlockChange location blockID

    0x0C -> do
      uuid <- decodeUUID
      action <- decodeVarInt
      case action of
        0 -> do
          title <- decodeText
          health <- decodeFloatBE
          color <- decodeVarInt
          division <- decodeVarInt
          flags <- Decode.anyWord8
          return $ CBBossBar uuid (BossBarAdd title health color division flags)
        1 -> do
          return $ CBBossBar uuid BossBarRemove
        2 -> do
          health <- decodeFloatBE
          return $ CBBossBar uuid (BossBarUpdateHealth health)
        3 -> do
          title <- decodeText
          return $ CBBossBar uuid (BossBarUpdateTitle title)
        4 -> do
          color <- decodeVarInt
          dividers <- decodeVarInt
          return $ CBBossBar uuid (BossBarUpdateStyle color dividers)
        5 -> do
          flags <- Decode.anyWord8
          return $ CBBossBar uuid (BossBarUpdateFlags flags)

        err -> fail $ "Error: Invalid BossBar action byte: " ++ show err

    0x0D -> do
      difficulty <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBServerDifficulty difficulty

    0x0E -> do
      count <- decodeVarInt
      matches <- V.replicateM count decodeText
      return $ CBTabComplete matches

    0x0F -> do
      jsonData <- decodeText
      position <- decodeInt8
      return $ CBChatMessage jsonData position

    0x10 -> do
      chunkX <- decodeInt32BE
      chunkZ <- decodeInt32BE
      recordCount <- decodeVarInt
      records <- V.replicateM recordCount decodeRecord
      return $ CBMultiBlockChange chunkX chunkZ records

    0x11 -> do
      windowID <- decodeInt8
      actionNumber <- decodeInt16BE
      accepted <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBConfirmTransaction windowID actionNumber accepted

    0x12 -> do
      windowID <- Decode.anyWord8
      return $ CBCloseWindow windowID

    0x13 -> do
      windowID <- Decode.anyWord8
      windowType <- decodeText
      windowTitle <- decodeText
      numberOfSlots <- Decode.anyWord8
      case windowType of
        "EntityHorse" -> do
          entityID <- decodeInt32BE
          return $ CBOpenWindow windowID (Left entityID) windowTitle numberOfSlots
        _ -> do
          return $ CBOpenWindow windowID (Right windowType) windowTitle numberOfSlots

    0x14 -> do
      windowID <- Decode.anyWord8
      count <- fmap fromEnum decodeInt16BE
      slotData <- V.replicateM count decodeSlot
      return $ CBWindowItems windowID slotData

    0x15 -> do
      windowID <- Decode.anyWord8
      property <- decodeInt16BE
      value <- decodeInt16BE
      return $ CBWindowProperty windowID property value

    0x16 -> do
      windowID <- decodeInt8
      slot <- decodeInt16BE
      dat <- decodeSlot
      return $ CBSetSlot windowID slot dat

    0x17 -> do
      itemID <- decodeVarInt
      cooldownTicks <- decodeVarInt
      return $ CBSetCooldown itemID cooldownTicks

    0x18 -> do
      channel <- decodeText
      dat <- Decode.takeByteString
      return $ CBPluginMessage channel dat

    0x19 -> do
      soundName <- decodeText
      soundCategory <- decodeVarInt
      effectPosX <- decodeInt32BE
      effectPosY <- decodeInt32BE
      effectPosZ <- decodeInt32BE
      volume <- decodeFloatBE
      pitch <- decodeFloatBE
      return $ CBNamedSoundEffect soundName soundCategory effectPosX effectPosY effectPosZ volume pitch

    0x1A -> do
      reason <- decodeText
      return $ CBPlayDisconnect reason

    0x1B -> do
      entityID <- decodeInt32BE
      entityStatus <- fmap (toEnum . fromEnum) decodeInt8
      return $ CBEntityStatus entityID entityStatus

    0x1C -> do
      x <- decodeFloatBE
      y <- decodeFloatBE
      z <- decodeFloatBE
      radius <- decodeFloatBE
      count <- fmap fromEnum decodeInt32BE
      records <- V.replicateM count (do a <- decodeInt8
                                        b <- decodeInt8
                                        c <- decodeInt8
                                        return (a,b,c))
      pMotionX <- decodeFloatBE
      pMotionY <- decodeFloatBE
      pMotionZ <- decodeFloatBE
      return $ CBExplosion x y z radius records pMotionX pMotionY pMotionZ

    0x1D -> do
      chunkX <- decodeInt32BE
      chunkZ <- decodeInt32BE
      return $ CBUnloadChunk chunkX chunkZ

    0x1E -> do
      reason <- fmap (toEnum . fromEnum) Decode.anyWord8
      value <- decodeFloatBE
      return $ CBChangeGameState reason value

    0x1F -> do
      keepAliveID <- decodeVarInt
      return $ CBKeepAlive keepAliveID

    0x20 -> do
      chunkX <- decodeInt32BE
      chunkZ <- decodeInt32BE
      groundUp <- fmap (toEnum . fromEnum) Decode.anyWord8
      primaryBitMask <- decodeVarInt
      if groundUp
        then do
          size <- decodeVarInt
          bs <- Decode.take (size - 256)
          let dat = Decode.parseOnly
                      ((fmap V.fromList (Decode.many' (decodeChunkSection primaryBitMask))) <* Decode.endOfInput)
                      bs
          biomes <- Decode.take 256
          count <- decodeVarInt
          blockEntities <- V.replicateM count decodeNBT
          case dat of
            Left err -> fail err
            Right dat' -> return $
                            CBChunkData
                              chunkX
                              chunkZ
                              groundUp
                              primaryBitMask
                              dat'
                              (Just biomes)
                              blockEntities
        else do
          size <- decodeVarInt
          bs <- Decode.take size
          let dat = Decode.parseOnly
                      ((fmap V.fromList (Decode.many' (decodeChunkSection primaryBitMask))) <* Decode.endOfInput)
                      bs
          count <- decodeVarInt
          blockEntities <- V.replicateM count decodeNBT
          case dat of
            Left err -> fail err
            Right dat' -> return $
                            CBChunkData
                              chunkX
                              chunkZ
                              groundUp
                              primaryBitMask
                              dat'
                              Nothing
                              blockEntities

    0x21 -> do
      effectID <- decodeInt32BE
      location <- decodePosition
      dat <- decodeInt32BE
      disableRelativeVolume <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBEffect effectID location dat disableRelativeVolume

    0x22 -> do
      particleID <- decodeInt32BE
      longDistance <- fmap (toEnum . fromEnum) Decode.anyWord8
      x <- decodeFloatBE
      y <- decodeFloatBE
      z <- decodeFloatBE
      offsetX <- decodeFloatBE
      offsetY <- decodeFloatBE
      offsetZ <- decodeFloatBE
      particleData <- decodeFloatBE
      particleFloat <- decodeInt32BE
      dat <- fmap V.fromList (Decode.many' decodeVarInt)
      return $ CBParticle particleID longDistance x y z offsetX offsetY offsetZ particleData particleFloat dat

    0x23 -> do
      entityID <- decodeInt32BE
      gameMode <- fmap (toEnum . fromEnum) Decode.anyWord8
      dimension <- fmap (toEnum . fromEnum) decodeInt32BE
      difficulty <- fmap (toEnum . fromEnum) Decode.anyWord8
      maxPlayers <- Decode.anyWord8
      levelType <- decodeText
      reducedDebugInfo <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBJoinGame entityID gameMode dimension difficulty maxPlayers levelType reducedDebugInfo

    0x24 -> do
      itemDamage <- decodeVarInt
      scale <- decodeInt8
      trackingPositon <- fmap (toEnum . fromEnum) Decode.anyWord8
      count <- decodeVarInt
      icons <- V.replicateM count decodeIcon
      columns <- decodeInt8
      if columns > 0
        then do
          rows <- decodeInt8
          x <- decodeInt8
          z <- decodeInt8
          ln <- decodeVarInt
          dat <- Decode.takeByteString
          return $ CBMap itemDamage scale trackingPositon icons (UpdatedColumns columns rows x z dat)
        else return $ CBMap itemDamage scale trackingPositon icons NoUpdatedColumns


    0x25 -> do
      entityID <- decodeVarInt
      dX <- decodeInt16BE
      dY <- decodeInt16BE
      dZ <- decodeInt16BE
      onGround <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBEntityRelativeMove entityID dX dY dZ onGround

    0x26 -> do
      entityID <- decodeVarInt
      dX <- decodeInt16BE
      dY <- decodeInt16BE
      dZ <- decodeInt16BE
      yaw <- decodeAngle
      pitch <- decodeAngle
      onGround <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBEntityLookAndRelativeMove entityID dX dY dZ yaw pitch onGround

    0x27 -> do
      entityID <- decodeVarInt
      yaw <- decodeAngle
      pitch <- decodeAngle
      onGround <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBEntityLook entityID yaw pitch onGround

    0x28 -> do
      entityID <- decodeVarInt
      return $ CBEntity entityID

    0x29 -> do
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      yaw <- decodeFloatBE
      pitch <- decodeFloatBE
      return $ CBVehicleMove x y z yaw pitch

    0x2A -> do
      location <- decodePosition
      return $ CBOpenSignEditor location

    0x2B -> do
      flags <- decodeInt8
      flyingSpeed <- decodeFloatBE
      fieldOfViewModifier <- decodeFloatBE
      return $ CBPlayerAbilities flags flyingSpeed fieldOfViewModifier

    0x2C -> do
      event <- decodeVarInt
      case event of
        0 -> do
          return $ CBCombatEvent EnterCombat
        1 -> do
          duration <- decodeVarInt
          entityID <- decodeInt32BE
          return $ CBCombatEvent (EndCombat duration entityID)
        2 -> do
          playerID <- decodeVarInt
          entityID <- decodeInt32BE
          message <- decodeText
          return $ CBCombatEvent (EntityDead playerID entityID message)
        err -> fail $ "Unrecognized combat event: " ++ show err


    0x2D -> do
      action <- decodeVarInt
      numberOfPlayers <- decodeVarInt
      players <- V.replicateM numberOfPlayers (decodePlayerListEntry action)
      return $ CBPlayerListItem action players

    0x2E -> do
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      yaw <- decodeFloatBE
      pitch <- decodeFloatBE
      flags <- decodeInt8
      teleportID <- decodeVarInt
      return $ CBPlayerPositionAndLook x y z yaw pitch flags teleportID

    0x2F -> do
      entityID <- decodeVarInt
      location <- decodePosition
      return $ CBUseBed entityID location

    0x30 -> do
      count <- decodeVarInt
      entityIDs <- V.replicateM count decodeVarInt
      return $ CBDestroyEntities entityIDs

    0x31 -> do
      entityID <- decodeVarInt
      effectID <- decodeInt8
      return $ CBRemoveEntityEffect entityID effectID

    0x32 -> do
      url <- decodeText
      hash <- decodeText
      return $ CBResourcePackSend url hash

    0x33 -> do
      dimension <- fmap (toEnum . fromEnum) decodeInt32BE
      difficulty <- fmap (toEnum . fromEnum) Decode.anyWord8
      gameMode <- fmap (toEnum . fromEnum) Decode.anyWord8
      levelType <- decodeText
      return $ CBRespawn dimension difficulty gameMode levelType

    0x34 -> do
      entityID <- decodeVarInt
      headYaw <- decodeAngle
      return $ CBEntityHeadLook entityID headYaw

    0x35 -> do
      action <- decodeVarInt
      case action of
        0 -> do
          diameter <- decodeDoubleBE
          return $ CBWorldBorder (SetSize diameter)
        1 -> do
          oldDiameter <- decodeDoubleBE
          newDiameter <- decodeDoubleBE
          speed <- decodeVarLong
          return $ CBWorldBorder (LerpSize oldDiameter newDiameter speed)
        2 -> do
          x <- decodeDoubleBE
          z <- decodeDoubleBE
          return $ CBWorldBorder (SetCenter x z)
        3 -> do
          x <- decodeDoubleBE
          z <- decodeDoubleBE
          oldDiameter <- decodeDoubleBE
          newDiameter <- decodeDoubleBE
          speed <- decodeVarLong
          portalBoundary <- decodeVarInt
          warningTime <- decodeVarInt
          warningBlocks <- decodeVarInt
          return $ CBWorldBorder (Initialize x z oldDiameter newDiameter speed portalBoundary warningTime warningBlocks)
        4 -> do
          warningTime <- decodeVarInt
          return $ CBWorldBorder (SetWarningTime warningTime)
        5 -> do
          warningBlocks <- decodeVarInt
          return $ CBWorldBorder (SetWarningBlocks warningBlocks)
        err -> fail $ "Unrecognized world border action: " ++ show err


    0x36 -> do
      cameraID <- decodeVarInt
      return $ CBCamera cameraID

    0x37 -> do
      slot <- decodeInt8
      return $ CBHeldItemChange slot

    0x38 -> do
      position <- decodeInt8
      scoreName <- decodeText
      return $ CBDisplayScoreboard position scoreName

    0x39 -> do
      entityID <- decodeVarInt
      metadata <- decodeEntityMetadata
      return $ CBEntityMetadata entityID metadata

    0x3A -> do
      attachedEntityID <- decodeInt32BE
      holdingEntityID <- decodeInt32BE
      return $ CBAttachEntity attachedEntityID holdingEntityID

    0x3B -> do
      entityID <- decodeVarInt
      vX <- decodeInt16BE
      vY <- decodeInt16BE
      vZ <- decodeInt16BE
      return $ CBEntityVelocity entityID vX vY vZ

    0x3C -> do
      entityID <- decodeVarInt
      slot <- decodeVarInt
      item <- decodeSlot
      return $ CBEntityEquipment entityID slot item

    0x3D -> do
      experienceBar <- decodeFloatBE
      level <- decodeVarInt
      totalExperience <- decodeVarInt
      return $ CBSetExperience experienceBar level totalExperience

    0x3E -> do
      health <- decodeFloatBE
      food <- decodeVarInt
      foodSaturation <- decodeFloatBE
      return $ CBUpdateHealth health food foodSaturation

    0x3F -> do
      objectiveName <- decodeText
      mode <- decodeInt8
      case mode of
        0 -> do
          objectiveValue <- decodeText
          t <- decodeText
          return $ CBScoreboardObjective objectiveName (CreateScoreboard objectiveValue t)
        1 -> do
          return $ CBScoreboardObjective objectiveName RemoveScoreboard
        2 -> do
          objectiveValue <- decodeText
          t <- decodeText
          return $ CBScoreboardObjective objectiveName (UpdateDisplayText objectiveValue t)
        err -> fail $ "Error: Invalid ScoreboardMode byte: " ++ show err

    0x40 -> do
      entityID <- decodeVarInt
      count <- decodeVarInt
      passengers <- V.replicateM count decodeVarInt
      return $ CBSetPassengers entityID passengers

    0x41 -> do
      teamName <- decodeText
      mode <- decodeInt8
      case mode of
        0 -> do
          displayName <- decodeText
          prefix <- decodeText
          suffix <- decodeText
          flags <- decodeInt8
          tagVisibility <- decodeText
          collision <- decodeText
          color <- decodeInt8
          count <- decodeVarInt
          players <- V.replicateM count decodeText
          return $ CBTeams teamName (CreateTeam displayName prefix suffix flags tagVisibility collision color players)
        1 -> do
          return $ CBTeams teamName RemoveTeam
        2 -> do
          displayName <- decodeText
          prefix <- decodeText
          suffix <- decodeText
          flags <- decodeInt8
          tagVisibility <- decodeText
          collision <- decodeText
          color <- decodeInt8
          return $ CBTeams teamName (UpdateTeamInfo displayName prefix suffix flags tagVisibility collision color)
        3 -> do
          count <- decodeVarInt
          players <- V.replicateM count decodeText
          return $ CBTeams teamName (AddPlayers players)
        4 -> do
          count <- decodeVarInt
          players <- V.replicateM count decodeText
          return $ CBTeams teamName (RemovePlayers players)
        err -> fail $ "Unrecognized team mode: " ++ show err

    0x42 -> do
      scoreName <- decodeText
      action <- decodeInt8
      objectiveName <- decodeText
      case action of
        0 -> do
          value <- decodeVarInt
          return $ CBUpdateScore (CreateOrUpdateScoreItem scoreName objectiveName value)
        1 -> do
          return $ CBUpdateScore (RemoveScoreItem scoreName objectiveName)
        err -> fail $ "Error: Invalid UpdateScoreAction byte: " ++ show err

    0x43 -> do
      location <- decodePosition
      return $ CBSpawnPosition location

    0x44 -> do
      worldAge <- decodeInt64BE
      timeOfDay <- decodeInt64BE
      return $ CBTimeUpdate worldAge timeOfDay

    0x45 -> do
      action <- decodeVarInt
      case action of
        0 -> do
          titleText <- decodeText
          return $ CBTitle (SetTitle titleText)
        1 -> do
          subtitleText <- decodeText
          return $ CBTitle (SetSubtitle subtitleText)
        2 -> do
          fadeIn <- decodeInt32BE
          stay <- decodeInt32BE
          fadeOut <- decodeInt32BE
          return $ CBTitle (SetTimesAndDisplay fadeIn stay fadeOut)
        3 -> return $ CBTitle Hide
        4 -> return $ CBTitle Reset
        err -> fail $ "Unrecognized title action: " ++ show err

    0x46 -> do
      soundID <- decodeVarInt
      soundCategory <- decodeVarInt
      effectPosX <- decodeInt32BE
      effectPosY <- decodeInt32BE
      effectPosZ <- decodeInt32BE
      volume <- decodeFloatBE
      pitch <- decodeFloatBE
      return $ CBSoundEffect soundID soundCategory effectPosX effectPosY effectPosZ volume pitch

    0x47 -> do
      header <- decodeText
      footer <- decodeText
      return $ CBPlayerListHeaderAndFooter header footer

    0x48 -> do
      collectedEntityID <- decodeVarInt
      collectorEntityID <- decodeVarInt
      return $ CBCollectItem collectedEntityID collectorEntityID

    0x49 -> do
      entityID <- decodeVarInt
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      yaw <- decodeAngle
      pitch <- decodeAngle
      onGround <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBEntityTeleport entityID x y z yaw pitch onGround

    0x4A -> do
      entityID <- decodeVarInt
      count <- fmap fromEnum decodeInt32BE
      properties <- V.replicateM count decodeEntityProperty
      return $ CBEntityProperties entityID properties

    0x4B -> do
      entityID <- decodeVarInt
      effectID <- decodeInt8
      amplifier <- decodeInt8
      duration <- decodeVarInt
      hideParticles <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ CBEntityEffect entityID effectID amplifier duration hideParticles

    err -> fail $ "Unrecognized packetID: " ++ show err


data SBPlay
  -- | __Teleport Confirm:__
  -- Sent by client as confirmation of Player Position And Look (Play, 0x2E, clientbound).
  = SBTeleportConfirm VarInt

  -- | __Tab-Complete (serverbound):__
  -- Sent when the user presses tab while writing text.
  | SBTabComplete T.Text Bool (Maybe Position)

  -- | __Chat Message (serverbound):__
  -- Used to send a chat message to the server. The message may not be longer than 100 characters or else the server will kick the client.
  --
  -- If the message starts with a /, the server will attempt to interpret it as a command. Otherwise, the server will broadcast the same chat message to all players on the server (including the player that sent the message), prepended with player's name. Specifically, it will respond with a translate chat component, "chat.type.text" with the first parameter set to the display name of the player (including some chat component logic to support clicking the name to send a PM) and the second
  -- parameter set to the message.
  | SBChatMessage T.Text

  -- | __Client Status:__
  -- Sent when the client is ready to complete login and when the client is ready to respawn after death.
  | SBClientStatus VarInt

  -- | __Client Settings:__
  -- Sent when the player connects, or when settings are changed.
  | SBClientSettings T.Text Int8 VarInt Bool Word8 VarInt

  -- | __Confirm Transaction (serverbound):__
  -- If a transaction sent by the client was not accepted, the server will reply with a Confirm Transaction (Play, 0x32, clientbound) packet with the Accepted field set to false. When this happens, the client must reflect the packet to apologize (as with movement), otherwise the server ignores any successive transactions.
  | SBConfirmTransaction Int8 Short Bool

  | SBEnchantItem Int8 Int8

  -- | __Click Window:__
  -- This packet is sent by the player when it clicks on a slot in a window.
  | SBClickWindow Word8 Short Int8 Short VarInt Slot

  -- | __Close Window:__
  -- This packet is sent by the client when closing a window.
  --
  -- Notchian clients send a Close Window packet with Window ID 0 to close their inventory even though there is never an Open Window packet for the inventory.
  | SBCloseWindow Word8

  -- | __Plugin Message (serverbound):
  -- Mods and plugins can use this to send their data. Minecraft itself uses a number of plugin channels. These internal channels are prefixed with MC|.
  --
  -- More documentation on this: http://dinnerbone.com/blog/2012/01/13/minecraft-plugin-channels-messaging/
  --
  -- Note that the length of Data is known only from the packet length, since the packet has no length field of any kind.
  | SBPluginMessage T.Text B.ByteString

  -- | __Use Entity:__
  -- This packet is sent from the client to the server when the client attacks or right-clicks another entity (a player, minecart, etc).
  --
  -- A Notchian server only accepts this packet if the entity being attacked/used is visible without obstruction and within a 4-unit radius of the player's position.
  --
  -- Note that middle-click in creative mode is interpreted by the client and sent as a Creative Inventory Action packet instead.
  | SBUseEntity VarInt UseEntityType

  -- | __Keep Alive (serverbound):__
  -- The server will frequently send out a keep-alive, each containing a random ID. The client must respond with the same packet.
  | SBKeepAlive VarInt

  -- | __Player Position:__
  -- Updates the player's XYZ position on the server.
  --
  -- Checking for moving too fast is achieved like this:
  --
  -- Each server tick, the player's current position is stored
  -- When a player moves, the changes in x, y, and z coordinates are compared with the positions from the previous tick (Δx, Δy, Δz)
  -- Total movement distance squared is computed as Δx² + Δy² + Δz²
  -- The expected movement distance squared is computed as velocityX² + veloctyY² + velocityZ²
  -- If the total movement distance squared value minus the expected movement distance squared value is more than 100 (300 if the player is using an elytra), they are moving too fast.
  -- If the player is moving too fast, it will be logged that "<player> moved too quickly! " followed by the change in x, y, and z, and the player will be teleported back to their current (before this packet) serverside position.
  --
  -- Also, if the absolute value of X or the absolute value of Z is a value greater than 3.2×107, or X, Y, or Z are not finite (either positive infinity, negative infinity, or NaN), the client will be kicked for “Invalid move player packet received”.
  | SBPlayerPosition Double Double Double Bool

  -- | __Player Position And Look (serverbound):__
  -- A combination of Player Look and Player Position.
  | SBPlayerPositionAndLook Double Double Double Float Float Bool

  -- | __Player Look:__
  -- Updates the direction the player is looking in.
  --
  -- Yaw is measured in degrees, and does not follow classical trigonometry rules. The unit circle of yaw on the XZ-plane starts at (0, 1) and turns counterclockwise, with 90 at (-1, 0), 180 at (0,-1) and 270 at (1, 0). Additionally, yaw is not clamped to between 0 and 360 degrees; any number is valid, including negative numbers and numbers greater than 360.
  --
  -- Pitch is measured in degrees, where 0 is looking straight ahead, -90 is looking straight up, and 90 is looking straight down.
  | SBPlayerLook Float Float Bool

  -- | __Player:__
  -- This packet as well as Player Position (Play, 0x04, serverbound), Player Look (Play, 0x05, serverbound), and Player Position And Look (Play, 0x06, serverbound) are called the “serverbound movement packets”. At least one of them must be sent on each tick to ensure that servers will update things like player health correctly. Vanilla clients will send Player Position once every 20 ticks even for a stationary player, and Player on every other tick.
  --
  -- This packet is used to indicate whether the player is on ground (walking/swimming), or airborne (jumping/falling).
  --
  -- When dropping from sufficient height, fall damage is applied when this state goes from false to true. The amount of damage applied is based on the point where it last changed from true to false. Note that there are several movement related packets containing this state.
  | SBPlayer Bool

  -- | __Vehicle Move (serverbound):__
  -- Sent when a player moves in a vehicle. Fields are the same as in Player Position And Look (Play, 0x2E, serverbound). Note that all fields use absolute positioning and do not allow for relative positioning.
  | SBVehicleMove Double Double Double Float Float

  -- | __Steer Boat:__
  -- Used to visually update whether boat paddles are turning. The server will update the Boat entity metadata to match the values here.
  | SBSteerBoat Bool Bool

  -- | __Player Abilities (serverbound):
  -- The latter 2 bytes are used to indicate the walking and flying speeds respectively, while the first byte is used to determine the value of 4 booleans.
  --
  -- The vanilla client sends this packet when the player starts/stops flying with the Flags parameter changed accordingly. All other parameters are ignored by the vanilla server.
  | SBPlayerAbilities Int8 Float Float

  -- | __Player Digging:__
  -- Sent when the player mines a block. A Notchian server only accepts digging packets with coordinates within a 6-unit radius between the center of the block and 1.5 units from the player's feet (not their eyes).
  | SBPlayerDigging VarInt Position Int8

  -- | __Entity Action:__
  -- Sent by the client to indicate that it has performed certain actions: sneaking (crouching), sprinting, exiting a bed, jumping with a horse, and opening a horse's inventory while riding it.
  | SBEntityAction VarInt VarInt VarInt

  | SBSteerVehicle Float Float Word8
  | SBResourcePackStatus VarInt

  -- | __Held Item Change (serverbound):__
  -- Sent when the player changes the slot selection.
  | SBHeldItemChange Short

  -- | __Creative Inventory Action:__
  -- While the user is in the standard inventory (i.e., not a crafting bench) in Creative mode, the player will send this packet.
  --
  -- Clicking in the creative inventory menu is quite different from non-creative inventory management. Picking up an item with the mouse actually deletes the item from the server, and placing an item into a slot or dropping it out of the inventory actually tells the server to create the item from scratch. (This can be verified by clicking an item that you don't mind deleting, then severing the connection to the server; the item will be nowhere to be found when you log back in.) As a result of
  -- this implementation strategy, the "Destroy Item" slot is just a client-side implementation detail that means "I don't intend to recreate this item.". Additionally, the long listings of items (by category, etc.) are a client-side interface for choosing which item to create. Picking up an item from such listings sends no packets to the server; only when you put it somewhere does it tell the server to create the item in that location.
  --
  -- This action can be described as "set inventory slot". Picking up an item sets the slot to item ID -1. Placing an item into an inventory slot sets the slot to the specified item. Dropping an item (by clicking outside the window) effectively sets slot -1 to the specified item, which causes the server to spawn the item entity, etc.. All other inventory slots are numbered the same as the non-creative inventory (including slots for the 2x2 crafting menu, even though they aren't visible in the
  -- vanilla client).
  | SBCreativeInventoryAction Short Slot

  -- | __Update Sign:__
  -- This message is sent from the client to the server when the “Done” button is pushed after placing a sign.
  --
  -- The server only accepts this packet after Open Sign Editor (Play, 0x2A, clientbound), otherwise this packet is silently ignored.
  | SBUpdateSign Position T.Text T.Text T.Text T.Text

  -- | __Animation (serverbound):__
  -- Sent when the player's arm swings.
  | SBAnimation VarInt

  -- | __Spectate:__
  -- Teleports the player to the given entity. The player must be in spectator mode.
  --
  -- The Notchian client only uses this to teleport to players, but it appears to accept any type of entity. The entity does not need to be in the same dimension as the player; if necessary, the player will be respawned in the right world. If the given entity cannot be found (or isn't loaded), this packet will be ignored. It will also be ignored if the player attempts to teleport to themselves.
  | SBSpectate UUID

  -- | __Player Block Placement:__
  -- In normal operation (i.e. placing a block), this packet is sent once, with the values set normally.
  --
  -- The Cursor Position X/Y/Z fields (also known as in-block coordinates) are calculated using raytracing. The unit corresponds to one pixel in the default resource pack. For example, let's say a slab is being placed against the south face of a full block. The Cursor Position X will be higher if the player was pointing near the right (east) edge of the face, lower if pointing near the left. The Cursor Position Y will be used to determine whether it will appear as a bottom slab (values 0–7) or
  -- as a top slab (values 8–15). The Cursor Position Z should be 15 since the player was looking at the southernmost part of the block.
  --
  -- This packet has a special case where X, Y, Z, and Face are all -1. (Note that Y is unsigned so set to 255.) This special packet indicates that the currently held item for the player should have its state updated such as eating food, pulling back bows, using buckets, etc.
  --
  -- Special note on using buckets: When using buckets, the Notchian client might send two packets: first a normal and then a special case. The first normal packet is sent when you're looking at a block (e.g. the water you want to scoop up). This normal packet does not appear to do anything with a Notchian server. The second, special case packet appears to perform the action — based on current position/orientation and with a distance check — it appears that buckets can only be used within a
  -- radius of 6 units.
  | SBPlayerBlockPlacement Position VarInt VarInt Word8 Word8 Word8

  -- | __Use Item:__
  -- Sent when pressing the Use Item key (default: right click) with an item in hand.
  | SBUseItem VarInt
  deriving (Show,Eq)


encodeSBPlay :: SBPlay -> Encode.Builder
encodeSBPlay (SBTeleportConfirm teleportID) =
  Encode.word8 0x00
  <> encodeVarInt teleportID

encodeSBPlay (SBTabComplete text assumeCommand lookedAtBlock) =
  Encode.word8 0x01
  <> encodeText text
  <> encodeBool assumeCommand
  <> case lookedAtBlock of
      Just lookedAtBlock' -> do
        encodeBool True
        <> encodePosition lookedAtBlock'
      Nothing -> do
        encodeBool False

encodeSBPlay (SBChatMessage message) =
  Encode.word8 0x02
  <> encodeText message

encodeSBPlay (SBClientStatus actionID) =
  Encode.word8 0x03
  <> encodeVarInt actionID

encodeSBPlay (SBClientSettings locale viewDistance chatMode chatColors displayedSkinParts mainHand) =
  Encode.word8 0x04
  <> encodeText locale
  <> Encode.int8 viewDistance
  <> encodeVarInt chatMode
  <> (Encode.word8 . toEnum . fromEnum $ chatColors)
  <> Encode.word8 displayedSkinParts
  <> encodeVarInt mainHand

encodeSBPlay (SBConfirmTransaction windowID actionNumber accepted) =
  Encode.word8 0x05
  <> Encode.int8 windowID
  <> Encode.int16BE actionNumber
  <> (Encode.word8 . toEnum . fromEnum $ accepted)

encodeSBPlay (SBEnchantItem windowID enchantment) =
  Encode.word8 0x06
  <> Encode.int8 windowID
  <> Encode.int8 enchantment

encodeSBPlay (SBClickWindow windowID slot button actionNumber mode clickedItem) =
  Encode.word8 0x07
  <> Encode.word8 windowID
  <> Encode.int16BE slot
  <> Encode.int8 button
  <> Encode.int16BE actionNumber
  <> encodeVarInt mode
  <> encodeSlot clickedItem

encodeSBPlay (SBCloseWindow windowID) =
  Encode.word8 0x08
  <> Encode.word8 windowID

encodeSBPlay (SBPluginMessage channel dat) =
  Encode.word8 0x09
  <> encodeText channel
  <> Encode.byteString dat

encodeSBPlay (SBUseEntity target t) =
  Encode.word8 0x0A
  <> encodeVarInt target
  <> case t of
      (InteractWithEntity h) -> do
        encodeVarInt 0
        <> encodeVarInt (fromEnum h)
      AttackEntity -> do
        encodeVarInt 1
      (InteractAtEntity tX tY tZ h) -> do
        encodeVarInt 2
        <> Encode.floatBE tX
        <> Encode.floatBE tY
        <> Encode.floatBE tZ
        <> encodeVarInt (fromEnum h)

encodeSBPlay (SBKeepAlive keepAliveID) =
  Encode.word8 0x0B
  <> encodeVarInt keepAliveID

encodeSBPlay (SBPlayerPosition x feetY z onGround) =
  Encode.word8 0x0C
  <> Encode.doubleBE x
  <> Encode.doubleBE feetY
  <> Encode.doubleBE z
  <> (Encode.word8 . toEnum . fromEnum $ onGround)

encodeSBPlay (SBPlayerPositionAndLook x feetY z yaw pitch onGround) =
  Encode.word8 0x0D
  <> Encode.doubleBE x
  <> Encode.doubleBE feetY
  <> Encode.doubleBE z
  <> Encode.floatBE yaw
  <> Encode.floatBE pitch
  <> (Encode.word8 . toEnum . fromEnum $ onGround)

encodeSBPlay (SBPlayerLook yaw pitch onGround) =
  Encode.word8 0x0E
  <> Encode.floatBE yaw
  <> Encode.floatBE pitch
  <> (Encode.word8 . toEnum . fromEnum $ onGround)

encodeSBPlay (SBPlayer onGround) =
  Encode.word8 0x0F
  <> (Encode.word8 . toEnum . fromEnum $ onGround)

encodeSBPlay (SBVehicleMove x y z yaw pitch) =
  Encode.word8 0x10
  <> Encode.doubleBE x
  <> Encode.doubleBE y
  <> Encode.doubleBE z
  <> Encode.floatBE yaw
  <> Encode.floatBE pitch

encodeSBPlay (SBSteerBoat rightPaddle leftPaddle) =
  Encode.word8 0x11
  <> (Encode.word8 . toEnum . fromEnum $ rightPaddle)
  <> (Encode.word8 . toEnum . fromEnum $ leftPaddle)

encodeSBPlay (SBPlayerAbilities flags flyingSpeed walkingSpeed) =
  Encode.word8 0x12
  <> Encode.int8 flags
  <> Encode.floatBE flyingSpeed
  <> Encode.floatBE walkingSpeed

encodeSBPlay (SBPlayerDigging status location face) =
  Encode.word8 0x13
  <> encodeVarInt status
  <> encodePosition location
  <> Encode.int8 face

encodeSBPlay (SBEntityAction entityID actionID jumpBoost) =
  Encode.word8 0x14
  <> encodeVarInt entityID
  <> encodeVarInt actionID
  <> encodeVarInt jumpBoost

encodeSBPlay (SBSteerVehicle sideways forward flags) =
  Encode.word8 0x15
  <> Encode.floatBE sideways
  <> Encode.floatBE forward
  <> Encode.word8 flags

encodeSBPlay (SBResourcePackStatus result) =
  Encode.word8 0x16
  <> encodeVarInt result

encodeSBPlay (SBHeldItemChange slot) =
  Encode.word8 0x17
  <> Encode.int16BE slot

encodeSBPlay (SBCreativeInventoryAction slot clickedItem) =
  Encode.word8 0x18
  <> Encode.int16BE slot
  <> encodeSlot clickedItem

encodeSBPlay (SBUpdateSign location line1 line2 line3 line4) =
  Encode.word8 0x19
  <> encodePosition location
  <> encodeText line1
  <> encodeText line2
  <> encodeText line3
  <> encodeText line4

encodeSBPlay (SBAnimation hand) =
  Encode.word8 0x1A
  <> encodeVarInt hand

encodeSBPlay (SBSpectate targetPlayer) =
  Encode.word8 0x1B
  <> encodeUUID targetPlayer

encodeSBPlay (SBPlayerBlockPlacement location face hand cursorPosX cursorPosY cursorPosZ) =
  Encode.word8 0x1C
  <> encodePosition location
  <> encodeVarInt face
  <> encodeVarInt hand
  <> Encode.word8 cursorPosX
  <> Encode.word8 cursorPosY
  <> Encode.word8 cursorPosZ

encodeSBPlay (SBUseItem hand) =
  Encode.word8 0x1D
  <> encodeVarInt hand


decodeSBPlay :: Decode.Parser SBPlay
decodeSBPlay = do
  packetID <- Decode.anyWord8
  case packetID of
    0x00  -> do
      teleportID <- decodeVarInt
      return $ SBTeleportConfirm teleportID

    0x01 -> do
      text <- decodeText
      assumeCommand <- decodeBool
      hasPosition <- decodeBool
      case hasPosition of
        False -> do
          return $
            SBTabComplete
              text
              assumeCommand
              Nothing
        True -> do
          lookedAtBlock <- decodePosition
          return $
            SBTabComplete
              text
              assumeCommand
              (Just lookedAtBlock)

    0x02 -> do
      message <- decodeText
      return $ SBChatMessage message

    0x03 -> do
      actionID <- decodeVarInt
      return $ SBClientStatus actionID

    0x04 -> do
      locale <- decodeText
      viewDistance <- decodeInt8
      chatMode <- decodeVarInt
      chatColors <- fmap (toEnum . fromEnum) Decode.anyWord8
      displayedSkinParts <- Decode.anyWord8
      mainHand <- decodeVarInt
      return $ SBClientSettings locale viewDistance chatMode chatColors displayedSkinParts mainHand

    0x05 -> do
      windowID <- decodeInt8
      actionNumber <- decodeInt16BE
      accepted <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ SBConfirmTransaction windowID actionNumber accepted

    0x06 -> do
      windowID <- decodeInt8
      enchantment <- decodeInt8
      return $ SBEnchantItem windowID enchantment

    0x07 -> do
      windowID <- Decode.anyWord8
      slot <- decodeInt16BE
      button <- decodeInt8
      actionNumber <- decodeInt16BE
      mode <- decodeVarInt
      clickedItem <- decodeSlot
      return $ SBClickWindow windowID slot button actionNumber mode clickedItem

    0x08 -> do
      windowID <- Decode.anyWord8
      return $ SBCloseWindow windowID

    0x09 -> do
      channel <- decodeText
      dat <- Decode.takeByteString
      return $ SBPluginMessage channel dat

    0x0A -> do
      target <- decodeVarInt
      t <- decodeVarInt
      case t of
        0 -> do
          hand <- decodeVarInt
          return $ SBUseEntity target (InteractWithEntity hand)
        1 -> do
          return $ SBUseEntity target AttackEntity
        2 -> do
          targetX <- decodeFloatBE
          targetY <- decodeFloatBE
          targetZ <- decodeFloatBE
          hand <- fmap toEnum decodeVarInt
          return $ SBUseEntity target (InteractAtEntity targetX targetY targetZ hand)
        err -> fail $ "Error: Could not decode SBUseEntity type: " ++ show err

    0x0B -> do
      keepAliveID <- decodeVarInt
      return $ SBKeepAlive keepAliveID

    0x0C -> do
      x <- decodeDoubleBE
      feetY <- decodeDoubleBE
      z <- decodeDoubleBE
      onGround <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ SBPlayerPosition x feetY z onGround

    0x0D -> do
      x <- decodeDoubleBE
      feetY <- decodeDoubleBE
      z <- decodeDoubleBE
      yaw <- decodeFloatBE
      pitch <- decodeFloatBE
      onGround <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ SBPlayerPositionAndLook x feetY z yaw pitch onGround

    0x0E -> do
      yaw <- decodeFloatBE
      pitch <- decodeFloatBE
      onGround <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ SBPlayerLook yaw pitch onGround

    0x0F -> do
      onGround <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ SBPlayer onGround

    0x10 -> do
      x <- decodeDoubleBE
      y <- decodeDoubleBE
      z <- decodeDoubleBE
      yaw <- decodeFloatBE
      pitch <- decodeFloatBE
      return $ SBVehicleMove x y z yaw pitch

    0x11 -> do
      rightPaddle <- fmap (toEnum . fromEnum) Decode.anyWord8
      leftPaddle <- fmap (toEnum . fromEnum) Decode.anyWord8
      return $ SBSteerBoat rightPaddle leftPaddle

    0x12 -> do
      flags <- decodeInt8
      flyingSpeed <- decodeFloatBE
      walkingSpeed <- decodeFloatBE
      return $ SBPlayerAbilities flags flyingSpeed walkingSpeed

    0x13 -> do
      status <- decodeVarInt
      location <- decodePosition
      face <- decodeInt8
      return $ SBPlayerDigging status location face

    0x14 -> do
      entityID <- decodeVarInt
      actionID <- decodeVarInt
      jumpBoost <- decodeVarInt
      return $ SBEntityAction entityID actionID jumpBoost

    0x15 -> do
      sideways <- decodeFloatBE
      forward <- decodeFloatBE
      flags <- Decode.anyWord8
      return $ SBSteerVehicle sideways forward flags

    0x16 -> do
      result <- decodeVarInt
      return $ SBResourcePackStatus result

    0x17 -> do
      slot <- decodeInt16BE
      return $ SBHeldItemChange slot

    0x18 -> do
      slot <- decodeInt16BE
      clickedItem <- decodeSlot
      return $ SBCreativeInventoryAction slot clickedItem

    0x19 -> do
      location <- decodePosition
      line1 <- decodeText
      line2 <- decodeText
      line3 <- decodeText
      line4 <- decodeText
      return $ SBUpdateSign location line1 line2 line3 line4

    0x1A -> do
      hand <- decodeVarInt
      return $ SBAnimation hand

    0x1B -> do
      targetPlayer <- decodeUUID
      return $ SBSpectate targetPlayer

    0x1C -> do
      location <- decodePosition
      face <- decodeVarInt
      hand <- decodeVarInt
      cursorPosX <- Decode.anyWord8
      cursorPosY <- Decode.anyWord8
      cursorPosZ <- Decode.anyWord8
      return $ SBPlayerBlockPlacement location face hand cursorPosX cursorPosY cursorPosZ

    0x1D -> do
      hand <- decodeVarInt
      return $ SBUseItem hand

    err -> fail $ "Unrecognized packetID: " ++ show err


