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
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import qualified  Data.Attoparsec.ByteString as Decode
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Char8 as BC
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as Encode
import            Data.Int
import            Data.Maybe
import            Data.Monoid
import            Data.NBT
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.Serialize
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word

import            OpenSandbox.Types
import            OpenSandbox.Protocol.Types


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
    <> (encodeVarInt . fromEnum $ nextState)
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
        nextState <- (fmap toEnum decodeVarInt)
        return $ SBHandshake protocolVersion srvAddress srvPort nextState
      0xfe  -> do
        payload <- Decode.anyWord8
        return $ SBLegacyServerListPing payload


data CBStatus
  = CBResponse T.Text
  | CBPong Int64
  deriving (Show,Eq)


encodeCBStatus :: CBStatus -> Encode.Builder
encodeCBStatus (CBResponse jsonResponse) =
    Encode.word8 0x00
    <> (encodeText jsonResponse)
encodeCBStatus (CBPong payload) =
    Encode.word8 0x01
    <> (Encode.word64BE . toEnum . fromEnum $ payload)


decodeCBStatus :: Decode.Parser CBStatus
decodeCBStatus = do
    packetID <- Decode.anyWord8
    case packetID of
      0x00  -> do
        jsonResponse <- decodeText
        return $ CBResponse jsonResponse
      0x01  -> do
        payload <- (fmap (toEnum . fromEnum) decodeWord64BE)
        return $ CBPong payload


data SBStatus
  = SBRequest
  | SBPing Int64
  deriving (Show,Eq)


encodeSBStatus :: SBStatus -> Encode.Builder
encodeSBStatus SBRequest =
  Encode.word8 0x00
encodeSBStatus (SBPing payload) =
  Encode.word8 0x01
  <> (Encode.word64BE . toEnum . fromEnum $ payload)


decodeSBStatus :: Decode.Parser SBStatus
decodeSBStatus = do
    packetID <- Decode.anyWord8
    case packetID of
      0x00  -> do
        return $ SBRequest
      0x01  -> do
        payload <- (fmap (toEnum . fromEnum) decodeWord64BE)
        return $ SBPing payload


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


data CBPlay

  -- | __Spawn Object:__
  -- Sent by the server when a vehicle or other object is created.
  = CBSpawnObject VarInt UUID Word8 Double Double Double Angle Angle Int32 Short Short Short

  -- | __Spawn Experience Orb:__
  -- Spawns one or more experience orbs.
  | CBSpawnExperienceOrb VarInt Double Double Double Int16

  -- | __Spawn Global Entity:__
  -- With this packet, the server notifies the client of thunderbolts striking within a 512 block radius around the player. The coordinates specify where exactly the thunderbolt strikes.
  | CBSpawnGlobalEntity VarInt Word8 Double Double Double

  -- | __Spawn Mob:__
  -- Sent by the server when a mob entity is spawned.
  | CBSpawnMob VarInt UUID Word8 Double Double Double Angle Angle Angle Short Short Short EntityMetadata

  -- | __Spawn Painting:__
  -- This packet shows location, name, and type of painting.
  | CBSpawnPainting VarInt UUID T.Text Position Word8

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
  | CBBlockBreakAnimation VarInt Position Word8

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
  | CBBlockAction Position BlockAction VarInt

  -- | __Block Change:__
  -- Fired whenever a block is changed within the render distance.
  | CBBlockChange Position VarInt

  | CBBossBar UUID BossBarAction

  -- | __Server Difficulty:__
  -- Changes the difficulty setting in the client's option menu.
  | CBServerDifficulty Difficulty

  -- | __Tab-Complete:__
  -- The server responds with a list of auto-completions of the
  -- last word sent to it.
  -- In the case of regular chat, this is a player username.
  -- Command names and parameters are also supported.
  | CBTabComplete (V.Vector T.Text)

  -- | __Chat Message (clientbound):__
  -- Identifying the difference between Chat/System Message is important as it helps respect the user's chat visibility options. While Position 2 accepts json formatting it will not display, old style formatting works.
  | CBChatMessage Chat Word8

  -- | __Multi Block Change:__
  -- Fired whenever 2 or more blocks are changed within the same chunk on the same tick.
  | CBMultiBlockChange Int32 Int32 (V.Vector BlockChange)

  -- | __Confirm Transaction (clientbound):__
  --  A packet from the server indicating whether a request from the client was accepted,
  --  or whether there was a conflict (due to lag).
  | CBConfirmTransaction Word8 Short Bool

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
  | CBOpenWindow Word8 T.Text Chat Word8 (Maybe Int32)

  -- | __Window Items:__
  -- Sent by the server when items in multiple slots (in a window) are added/removed.
  -- This includes the main inventory, equipped armour and crafting slots.
  | CBWindowItems Word8 (V.Vector Slot)

  -- | __Window Property:__
  -- This packet is used to inform the client that part of a GUI window should be updated.
  | CBWindowProperty Word8 Short Short

  -- | __Set Slot:__
  -- Sent by the server when an item in a slot (in a window) is added/removed.
  | CBSetSlot Word8 Short Slot

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
  | CBExplosion Float Float Float Float (V.Vector (Word8,Word8,Word8)) Float Float Float

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
  | CBChunkData Int32 Int32 Bool VarInt (V.Vector ChunkSection) (Maybe B.ByteString) (V.Vector NBT)

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
  | CBMap VarInt Word8 Bool (V.Vector Icon) Word8 (Maybe Word8) (Maybe Word8) (Maybe Word8) (Maybe B.ByteString)

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
  | CBLook VarInt Angle Angle Bool

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
  | CBPlayerAbilities Word8 Float Float

  | CBCombatEvent CombatEvent

  -- | __Player List Item:__
  -- Sent by the server to update the user list (<tab> in the client).
  | CBPlayerListItem VarInt (V.Vector Player)

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
  | CBPlayerPositionAndLook Double Double Double Float Float Word8 VarInt

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

  | CBRemoveEntityEffect VarInt Word8
  | CBResourcePackSend T.Text B.ByteString

  -- | __Respawn:__
  -- To change the player's dimension (overworld/nether/end), send them a respawn packet with the appropriate dimension, followed by prechunks/chunks for the new dimension, and finally a position and look packet. You do not need to unload chunks, the client will do it automatically.
  | CBRespawn Dimension Difficulty GameMode T.Text

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
  | CBHeldItemChange Word8

  -- | __Display Scoreboard:__
  -- This is sent to the client when it should display a scoreboard.
  | CBDisplayScoreboard Word8 T.Text

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
  | CBScoreboardObjective T.Text Word8 (Maybe T.Text) (Maybe T.Text)

  | CBSetPassengers VarInt VarInt (V.Vector VarInt)

  -- | __Teams:__
  -- Creates and updates teams.
  | CBTeams T.Text Word8 TeamMode

  -- | __Update Score:__
  -- This is sent to the client when it should update a scoreboard item.
  | CBUpdateScore T.Text Word8 T.Text (Maybe VarInt)

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

  | CBTitle Int TitleAction

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
  | CBEntityEffect VarInt Word8 Word8 VarInt Word8
  deriving (Show,Eq)


encodeCBPlay :: CBPlay -> Encode.Builder
encodeCBPlay (CBSpawnObject entityID objectUUID entityType x y z pitch yaw dat vX vY vZ) =
  Encode.word8 0x00
  <> encodeVarInt entityID
  <> encodeUUID objectUUID
  <> Encode.word8 entityType
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
  <> Encode.word8 entityType
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
  <> Encode.word8 direction

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
  <> encodeVarInt entityID
  <> (Encode.word8 . toEnum . fromEnum $ animation)

encodeCBPlay (CBStatistics statistics) =
  Encode.word8 0x07
  <> (encodeVarInt . V.length $ statistics)
  <> (V.foldl1' (<>) (fmap encodeStatistic statistics))

encodeCBPlay (CBBlockBreakAnimation entityID location destroyStage) =
  Encode.word8 0x08
  <> encodeVarInt entityID
  <> encodePosition location
  <> Encode.word8 destroyStage

encodeCBPlay (CBUpdateBlockEntity location action nbtData) =
  Encode.word8 0x09
  <> encodePosition location
  <> (Encode.word8 . toEnum . fromEnum $ action)
  <> encodeNBT nbtData

encodeCBPlay (CBBlockAction location blockAction blockType) =
  Encode.word8 0x0A
  <> encodePosition location
  <> (case blockAction of
      NoteBlockAction instrumentType notePitch  -> do
        (Encode.word8 . toEnum . fromEnum $ instrumentType)
        <> (Encode.word8 . toEnum . fromEnum $ notePitch)
      PistonBlockAction pistonState pistonDirection -> do
        (Encode.word8 . toEnum . fromEnum $ pistonState)
        <> (Encode.word8 . toEnum . fromEnum $ pistonDirection)
      ChestBlockAction byte -> do
        Encode.word8 byte
    )
  <> encodeVarInt blockType

encodeCBPlay (CBBlockChange location blockID) =
  Encode.word8 0x0B
  <> encodePosition location
  <> encodeVarInt blockID

encodeCBPlay (CBBossBar uuid bossBarAction) =
  Encode.word8 0x0C
  <> encodeUUID uuid
  <> (case bossBarAction of
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
        <> encodeVarInt color
        <> encodeVarInt dividers

      BossBarUpdateFlags flags -> do
        encodeVarInt 5
        <> Encode.word8 flags
    )

encodeCBPlay (CBServerDifficulty difficulty) =
  Encode.word8 0x0D
  <> (Encode.word8 . toEnum . fromEnum $ difficulty)

encodeCBPlay (CBTabComplete matches) =
  Encode.word8 0x0E
  <> (encodeVarInt . V.length $ matches)
  <> V.foldl1' (<>) (fmap encodeText matches)

encodeCBPlay (CBChatMessage jsonData position) =
  Encode.word8 0x0F
  <> encodeText jsonData
  <> Encode.word8 position

encodeCBPlay (CBMultiBlockChange chunkX chunkZ records) =
  Encode.word8 0x10
  <> Encode.int32BE chunkX
  <> Encode.int32BE chunkZ
  <> (encodeVarInt . V.length $ records)
  <> V.foldl1' (<>) (fmap encodeRecord records)

encodeCBPlay (CBConfirmTransaction windowID actionNumber accepted) =
  Encode.word8 0x11
  <> Encode.word8 windowID
  <> Encode.int16BE actionNumber
  <> (Encode.word8 . toEnum . fromEnum $ accepted)

encodeCBPlay (CBCloseWindow windowID) =
  Encode.word8 0x12
  <> Encode.word8 windowID

encodeCBPlay (CBOpenWindow windowID windowType windowTitle numOfSlots entityID) =
  Encode.word8 0x13
  <> Encode.word8 windowID
  <> encodeText windowType
  <> encodeText windowTitle
  <> Encode.word8 numOfSlots
  <> case (windowType,entityID) of
      ("EntityHorse",Just eid)   -> Encode.int32BE eid
      _               -> mempty

encodeCBPlay (CBWindowItems windowID slotData) =
  Encode.word8 0x14
  <> Encode.word8 windowID
  <> (Encode.int16BE . toEnum . V.length $ slotData)
  <> V.foldl1' (<>) (fmap encodeSlot slotData)

encodeCBPlay (CBWindowProperty windowID property value) =
  Encode.word8 0x15
  <> Encode.word8 windowID
  <> Encode.int16BE property -- Should move to WindowProperty
  <> Encode.int16BE value

encodeCBPlay (CBSetSlot windowID slot slotData) =
  Encode.word8 0x16
  <> Encode.word8 windowID
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
  <> (Encode.word8 . toEnum . fromEnum $ entityStatus)

encodeCBPlay (CBExplosion x y z radius records pMotionX pMotionY pMotionZ) =
  Encode.word8 0x1C
  <> Encode.floatBE x
  <> Encode.floatBE y
  <> Encode.floatBE z
  <> Encode.floatBE radius
  <> (Encode.int32BE . toEnum . V.length $ records)
  <> V.foldl1' (<>) (fmap (\(a,b,c) -> Encode.word8 a <> Encode.word8 b <> Encode.word8 c) records)
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
  <> V.foldl1' (<>) (fmap encodeChunkSection dat)
  <> (case (groundUpCont,biomes) of
      (True,Just b)   -> Encode.byteString b
      _               -> mempty
    )
  <> (encodeVarInt . V.length $ blockEntities)
  <> V.foldl1' (<>) (fmap encodeNBT blockEntities)

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
  <> V.foldl1' (<>) (fmap encodeVarInt dat)

encodeCBPlay (CBJoinGame entityID gameMode dimension difficulty maxPlayers levelType reduceDebug) =
  Encode.word8 0x23
  <> Encode.int32BE entityID
  <> Encode.word8 gameMode
  <> Encode.int32BE dimension
  <> Encode.word8 difficulty
  <> Encode.word8 maxPlayers
  <> encodeText levelType
  <> (Encode.word8 . toEnum . fromEnum $ reduceDebug)

encodeCBPlay (CBMap itemDamage scale trackingPosition icons columns rows x z dat) =
  Encode.word8 0x24
  <> encodeVarInt itemDamage
  <> Encode.word8 scale
  <> (Encode.word8 . toEnum . fromEnum $ trackingPosition)
  <> (encodeVarInt . V.length $ icons)
  <> V.foldl1' (<>) (fmap encodeIcon icons)
  <> Encode.word8 columns
  <> case (columns,rows,x,z,dat) of
      (columns,Just rows',Just x',Just z', Just dat') -> do
        if columns > 0
          then Encode.word8 rows'
                <> Encode.word8 x'
                <> Encode.word8 z'
                <> (encodeVarInt . B.length $ dat')
                <> Encode.byteString dat'
          else mempty
      _ -> undefined

encodeCBPlay _ = undefined


data SBPlay
  -- | __Teleport Confirm:__
  -- Sent by client as confirmation of Player Position And Look (Play, 0x2E, clientbound).
  = SBTeleportConfirm VarInt

  -- | __Tab-Complete (serverbound):__
  -- Sent when the user presses tab while writing text.
  | SBTabComplete T.Text Bool Bool (Maybe Position)

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
  | SBClientSettings T.Text Word8 VarInt Bool Word8 VarInt

  -- | __Confirm Transaction (serverbound):__
  -- If a transaction sent by the client was not accepted, the server will reply with a Confirm Transaction (Play, 0x32, clientbound) packet with the Accepted field set to false. When this happens, the client must reflect the packet to apologize (as with movement), otherwise the server ignores any successive transactions.
  | SBConfirmTransaction Word8 Short Bool

  | SBEnchantItem Word8 Word8

  -- | __Click Window:__
  -- This packet is sent by the player when it clicks on a slot in a window.
  | SBClickWindow Word8 Short Word8 Short VarInt Slot

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
  | SBUseEntity VarInt VarInt (Maybe Float) (Maybe Float) (Maybe Float) (Maybe VarInt)

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
  | SBPlayerAbilities Word8 Float Float

  -- | __Player Digging:__
  -- Sent when the player mines a block. A Notchian server only accepts digging packets with coordinates within a 6-unit radius between the center of the block and 1.5 units from the player's feet (not their eyes).
  | SBPlayerDigging VarInt Position Word8

  -- | __Entity Action:__
  -- Sent by the client to indicate that it has performed certain actions: sneaking (crouching), sprinting, exiting a bed, jumping with a horse, and opening a horse's inventory while riding it.
  | SBEntityAction VarInt VarInt VarInt

  | SBSteerVehicle Float Float Word8
  | SBResourcePackStatus B.ByteString VarInt

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
