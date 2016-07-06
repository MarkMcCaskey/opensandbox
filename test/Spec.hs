{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import            Control.Monad
import qualified  Data.Array.IArray as IA
import            Data.Array.Unboxed (listArray)
import            Data.Attoparsec.ByteString
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as BB
import            Data.Either
import            Data.Int
import            Data.NBT
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import qualified  Data.Vector as V
import qualified  Data.Vector.Unboxed as U
import            Data.Word
import            OpenSandbox
import            Test.Hspec
import            Test.QuickCheck
import            GHC.Generics


instance Arbitrary (U.Vector Int32) where
  arbitrary = do
    ln <- choose (0,10) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ U.fromList e


instance Arbitrary (U.Vector Int8) where
  arbitrary = do
    ln <- choose (0,10) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ U.fromList e


instance Arbitrary UpdatedColumns where
  arbitrary = do
    columns <- (arbitrary :: Gen Int8)
    if columns > 0
      then UpdatedColumns <$> return columns <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      else return NoUpdatedColumns


instance Arbitrary UpdateScoreAction where
  arbitrary = do
    action <- arbitrary :: Gen Bool
    if action
      then CreateOrUpdateScoreItem <$> arbitrary <*> arbitrary <*> arbitrary
      else RemoveScoreItem <$> arbitrary <*> arbitrary


instance Arbitrary NBT where
  arbitrary = sized nbt'
    where nbt' 0 = return $ TagByte "" 5
          nbt' n | n > 0 = do
            tagID <- choose (0x01,0x0b) :: Gen Word8

            case tagID of
              0x01 -> TagByte <$> arbitrary <*> arbitrary
              0x02 -> TagShort <$> arbitrary <*> arbitrary
              0x03 -> TagInt <$> arbitrary <*> arbitrary
              0x04 -> TagLong <$> arbitrary <*> arbitrary
              0x05 -> TagFloat <$> arbitrary <*> arbitrary
              0x06 -> TagDouble <$> arbitrary <*> arbitrary
              0x07 -> TagByteArray <$> arbitrary <*> arbitrary
              0x08 -> TagString <$> arbitrary <*> arbitrary
              0x09 -> TagList <$> arbitrary <*> arbitrary
              0x0a -> TagCompound <$> arbitrary <*> vectorOf (n `div` 30) arbitrary
              0x0b -> TagIntArray <$> arbitrary <*> arbitrary


instance Arbitrary NBTList where
  arbitrary = sized nbtlst'
    where nbtlst' 0 = return $ NBTList TypeByte [NTagByte 0]
          nbtlst' n | n > 0 = do
            tagID <- choose (0x01,0x0b) :: Gen Word8
            i <- choose (0,5) :: Gen Int
            case tagID of
              0x01 -> NBTList <$> return TypeByte <*> vectorOf i (NTagByte <$> (arbitrary :: Gen Int8))
              0x02 -> NBTList <$> return TypeShort <*> vectorOf i (NTagShort <$> (arbitrary :: Gen Int16))
              0x03 -> NBTList <$> return TypeInt <*> vectorOf i (NTagInt <$> (arbitrary :: Gen Int32))
              0x04 -> NBTList <$> return TypeLong <*> vectorOf i (NTagLong <$> (arbitrary :: Gen Int64))
              0x05 -> NBTList <$> return TypeFloat <*> vectorOf i (NTagFloat <$> (arbitrary :: Gen Float))
              0x06 -> NBTList <$> return TypeDouble <*> vectorOf i (NTagDouble <$> (arbitrary :: Gen Double))
              0x07 -> NBTList <$> return TypeByteArray <*> vectorOf (n `div` 20) (NTagByteArray <$> (arbitrary :: Gen (U.Vector Int8)))
              0x08 -> NBTList <$> return TypeString <*> vectorOf i (NTagString <$> (arbitrary :: Gen T.Text))
              0x09 -> NBTList <$> return TypeList <*> vectorOf (n `div` 20) (NTagList <$> arbitrary)
              0x0a -> NBTList <$> return TypeCompound <*> vectorOf (n `div` 30) (NTagCompound <$> vectorOf (n `div` 30) (arbitrary :: Gen NBT))
              0x0b -> NBTList <$> return TypeIntArray <*> vectorOf (n `div` 20) (NTagIntArray <$> (arbitrary :: Gen (U.Vector Int32)))


instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList $ listOf arbitrary


instance Arbitrary UUID where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ fromWords a b c d


instance Arbitrary TitleAction where
  arbitrary = do
    k <- choose (1,5) :: Gen Int
    case k of
      1 -> SetTitle <$> arbitrary
      2 -> SetSubtitle <$> arbitrary
      3 -> SetTimesAndDisplay <$> arbitrary <*> arbitrary <*> arbitrary
      4 -> return Hide
      5 -> return Reset


instance Arbitrary TeamMode where
  arbitrary = do
    k <- choose (1,5) :: Gen Int
    case k of
      1 -> CreateTeam
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
      2 -> return $ RemoveTeam
      3 -> UpdateTeamInfo
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
      4 -> AddPlayers <$> arbitrary
      5 -> RemovePlayers <$> arbitrary


instance Arbitrary EntityProperty where
  arbitrary = EntityProperty <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary WorldBorderAction where
  arbitrary = do
    k <- choose (1,6) :: Gen Int
    case k of
      1 -> SetSize <$> arbitrary
      2 -> LerpSize <$> arbitrary <*> arbitrary <*> arbitrary
      3 -> SetCenter <$> arbitrary <*> arbitrary
      4 -> Initialize
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
      5 -> SetWarningTime <$> arbitrary
      6 -> SetWarningBlocks <$> arbitrary


instance Arbitrary Icon where
  arbitrary = Icon <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary CombatEvent where
  arbitrary = do
    k <- choose (1,3) :: Gen Int
    case k of
      1 -> return $ EnterCombat
      2 -> EndCombat <$> arbitrary <*> arbitrary
      3 -> EntityDead <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary EntityStatus where
  arbitrary = fmap toEnum (choose (0,34) :: Gen Int)


instance Arbitrary BlockChange where
  arbitrary = BlockChange <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary GameChangeReason where
  arbitrary = fmap toEnum (choose (0,9) :: Gen Int)


instance Arbitrary ChunkSection where
  arbitrary = do
    light <- choose (True,False)
    if light
      then OverworldChunkSection <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      else OtherChunkSection <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary DifficultyField where
  arbitrary = fmap toEnum (choose (0,3) :: Gen Int)


instance Arbitrary GameModeField where
  arbitrary = elements [SurvivalField,CreativeField,AdventureField,SpectatorField]


instance Arbitrary DimensionField where
  arbitrary = elements [OverworldField,NetherField,EndField]


instance Arbitrary WorldType where
  arbitrary = elements [Default,Flat,LargeBiomes,Amplified]


instance Arbitrary Statistic where
  arbitrary = do
    a <- arbitrary :: Gen T.Text
    b <- arbitrary :: Gen Int
    return $ Statistic a b


instance Arbitrary PlayerListEntry where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ PlayerListEntry a b


instance Arbitrary NextState where
  arbitrary = do
    switch <- choose (1,2) :: Gen Int
    case switch of
      1 -> return ProtocolStatus
      2 -> return ProtocolLogin


instance Arbitrary PlayerListAction where
  arbitrary = do
    switch <- choose (0,4) :: Gen Int
    case switch of
      0 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ PlayerListAdd a b c d e
      1 -> do
        a <- arbitrary
        return $ PlayerListUpdateGameMode a
      2 -> do
        a <- arbitrary
        return $ PlayerListUpdateLatency a
      3 -> do
        a <- arbitrary
        return $ PlayerListUpdateDisplayName a
      4 -> do
        return $ PlayerListRemovePlayer


instance Arbitrary PlayerProperty where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ PlayerProperty a b c


instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary


instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary


instance Arbitrary EntityMetadataEntry where
  arbitrary = do
    k <- arbitrary :: Gen Bool
    w <- choose (0,254) :: Gen Word8
    if k
      then return MetadataEnd
      else Entry <$> return w <*> arbitrary <*> arbitrary


instance Arbitrary MetadataType where
  arbitrary = fmap toEnum (choose (0,12) :: Gen Int)


instance Arbitrary Animation where
  arbitrary = fmap toEnum (choose (0,5) :: Gen Int)


instance Arbitrary UpdateBlockEntityAction where
  arbitrary = fmap toEnum (choose (1,9) :: Gen Int)


instance Arbitrary BlockAction where
  arbitrary = do
    k <- choose (0,2) :: Gen Int
    case k of
      0 -> NoteBlockAction <$> arbitrary <*> arbitrary
      1 -> PistonBlockAction <$> arbitrary <*> arbitrary
      2 -> ChestBlockAction <$> arbitrary


instance Arbitrary InstrumentType where
  arbitrary = fmap toEnum (choose (0,4) :: Gen Int)


instance Arbitrary NotePitch where
  arbitrary = fmap toEnum (choose (0,24) :: Gen Int)


instance Arbitrary PistonState where
  arbitrary = fmap toEnum (choose (0,1) :: Gen Int)


instance Arbitrary PistonDirection where
  arbitrary = fmap toEnum (choose (0,5) :: Gen Int)


instance Arbitrary BossBarAction where
  arbitrary = do
    k <- choose (0,5) :: Gen Int
    case k of
      0 -> BossBarAdd <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      1 -> return BossBarRemove
      2 -> BossBarUpdateHealth <$> arbitrary
      3 -> BossBarUpdateTitle <$> arbitrary
      4 -> BossBarUpdateStyle <$> arbitrary <*> arbitrary
      5 -> BossBarUpdateFlags <$> arbitrary


instance Arbitrary Slot where
  arbitrary = mkSlot <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary UseEntityType where
  arbitrary = do
    t <- (choose (0,2) :: Gen Int)
    case t of
      0 -> InteractWithEntity <$> arbitrary
      1 -> return AttackEntity
      2 -> InteractAtEntity <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary EntityHand where
  arbitrary = fmap toEnum (choose (0,1) :: Gen Int)


instance Arbitrary ScoreboardMode where
  arbitrary = do
    k <- choose (0,2) :: Gen Int
    case k of
      0 -> CreateScoreboard <$> arbitrary <*> arbitrary
      1 -> return RemoveScoreboard
      2 -> UpdateDisplayText <$> arbitrary <*> arbitrary


instance Arbitrary SBHandshaking where
  arbitrary = do
    v <- arbitrary
    a <- arbitrary
    p <- arbitrary
    s <- arbitrary
    return $ SBHandshake v a p s


instance Arbitrary SBStatus where
  arbitrary = do
    packetID <- elements [0..1] :: Gen Int
    case packetID of

      0x00 -> do
        return SBRequest

      0x01 -> do
        a <- arbitrary
        return $ SBPing a


instance Arbitrary CBStatus where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Word8
    case packetID of

      0x00  -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBResponse a b c d e

      0x01  -> do
        a <- arbitrary
        return $ CBPong a


instance Arbitrary CBLogin where
  arbitrary = do
    packetID <- choose (0x00,0x03) :: Gen Word8
    case packetID of

      0 -> do
        a <- arbitrary
        return $ CBLoginDisconnect a

      1 -> do
        a <- arbitrary
        b <- fmap B.pack arbitrary
        c <- fmap B.pack arbitrary
        return $ CBEncryptionRequest a b c

      2 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBLoginSuccess a b

      3 -> do
        a <- arbitrary
        return $ CBSetCompression a


instance Arbitrary SBLogin where
  arbitrary = do
    packetID <- choose (0x00,0x01) :: Gen Word8
    case packetID of

      0 -> do
        a <- arbitrary
        return $ SBLoginStart a

      1 -> do
        a <- fmap B.pack arbitrary
        b <- fmap B.pack arbitrary
        return $ SBEncryptionResponse a b


instance Arbitrary CBPlay where
  arbitrary = do
    packetID <- choose (0x00,0x4B) :: Gen Word8
    case packetID of
      0x00 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        i <- arbitrary
        j <- arbitrary
        k <- arbitrary
        l <- arbitrary
        return $ CBSpawnObject a b c d e f g h i j k l

      0x01 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBSpawnExperienceOrb a b c d e

      0x02 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBSpawnGlobalEntity a b c d e

      0x03 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        i <- arbitrary
        j <- arbitrary
        k <- arbitrary
        l <- arbitrary
        m <- arbitrary
        return $ CBSpawnMob a b c d e f g h i j k l m

      0x04 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBSpawnPainting a b c d e

      0x05 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        return $ CBSpawnPlayer a b c d e f g h

      0x06 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBAnimation a b

      0x07 -> do
        a <- arbitrary
        return $ CBStatistics a

      0x08 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBBlockBreakAnimation a b c

      0x09 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBUpdateBlockEntity a b c

      0x0A -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBBlockAction a b

      0x0B -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBBlockChange a b

      0x0C -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBBossBar a b

      0x0D -> do
        a <- arbitrary
        return $ CBServerDifficulty a

      0x0E -> do
        a <- arbitrary
        return $ CBTabComplete a

      0x0F -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBChatMessage a b

      0x10 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBMultiBlockChange a b c

      0x11 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBConfirmTransaction a b c

      0x12 -> do
        a <- arbitrary
        return $ CBCloseWindow a

      0x13 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBOpenWindow a b c d

      0x14 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBWindowItems a b

      0x15 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBWindowProperty a b c

      0x16 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBSetSlot a b c

      0x17 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBSetCooldown a b

      0x18 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBPluginMessage a b

      0x19 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBNamedSoundEffect a b c d e f g

      0x1A -> do
        a <- arbitrary
        return $ CBPlayDisconnect a

      0x1B -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBEntityStatus a b

      0x1C -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        return $ CBExplosion a b c d e f g h

      0x1D -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBUnloadChunk a b

      0x1E -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBChangeGameState a b

      0x1F -> do
        a <- arbitrary
        return $ CBKeepAlive a

      0x20 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ CBChunkData a b c d e f

      0x21 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBEffect a b c d

      0x22 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        i <- arbitrary
        j <- arbitrary
        k <- arbitrary
        return $ CBParticle a b c d e f g h i j k

      0x23 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBJoinGame a b c d e f g

      0x24 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBMap a b c d e

      0x25 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBEntityRelativeMove a b c d e

      0x26 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBEntityLookAndRelativeMove a b c d e f g

      0x27 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBEntityLook a b c d

      0x28 -> do
        a <- arbitrary
        return $ CBEntity a

      0x29 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBVehicleMove a b c d e

      0x2A -> do
        a <- arbitrary
        return $ CBOpenSignEditor a

      0x2B -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBPlayerAbilities a b c

      0x2C -> do
        a <- arbitrary
        return $ CBCombatEvent a

      0x2D -> do
        a <- arbitrary
        i <- arbitrary
        b <- fmap V.fromList $ vectorOf i arbitrary
        return $ CBPlayerListItem a b

      0x2E -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBPlayerPositionAndLook a b c d e f g

      0x2F -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBUseBed a b

      0x30 -> do
        a <- arbitrary
        return $ CBDestroyEntities a

      0x31 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBRemoveEntityEffect a b

      0x32 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBResourcePackSend a b

      0x33 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBRespawn a b c d

      0x34 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBEntityHeadLook a b

      0x35 -> do
        a <- arbitrary
        return $ CBWorldBorder a

      0x36 -> do
        a <- arbitrary
        return $ CBCamera a

      0x37 -> do
        a <- arbitrary
        return $ CBHeldItemChange a

      0x38 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBDisplayScoreboard a b

      0x39 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBEntityMetadata a b

      0x3A -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBAttachEntity a b

      0x3B -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBEntityVelocity a b c d

      0x3C -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBEntityEquipment a b c

      0x3D -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBSetExperience a b c

      0x3E -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBUpdateHealth a b c

      0x3F -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBScoreboardObjective a b

      0x40 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBSetPassengers a b

      0x41 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBTeams a b

      0x42 -> do
        a <- arbitrary
        return $ CBUpdateScore a

      0x43 -> do
        a <- arbitrary
        return $ CBSpawnPosition a

      0x44 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBTimeUpdate a b

      0x45 -> do
        a <- arbitrary
        return $ CBTitle a

      0x46 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBSoundEffect a b c d e f g

      0x47 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBPlayerListHeaderAndFooter a b

      0x48 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBCollectItem a b

      0x49 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBEntityTeleport a b c d e f g

      0x4A -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBEntityProperties a b

      0x4B -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBEntityEffect a b c d e


instance Arbitrary SBPlay where
  arbitrary = do
    packetID <- choose (0x00,0x1D) :: Gen Word8
    case packetID of
      0x00 -> do
        a <- arbitrary
        return $ SBTeleportConfirm a

      0x01 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBTabComplete a b c

      0x02 -> do
        a <- arbitrary
        return $ SBChatMessage a

      0x03 -> do
        a <- arbitrary
        return $ SBClientStatus a

      0x04 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ SBClientSettings a b c d e f

      0x05 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBConfirmTransaction a b c

      0x06 -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBEnchantItem a b

      0x07 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ SBClickWindow a b c d e f

      0x08 -> do
        a <- arbitrary
        return $ SBCloseWindow a

      0x09 -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBPluginMessage a b

      0x0A -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBUseEntity a b

      0x0B -> do
        a <- arbitrary
        return $ SBKeepAlive a

      0x0C -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ SBPlayerPosition a b c d

      0x0D -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ SBPlayerPositionAndLook a b c d e f

      0x0E -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBPlayerLook a b c

      0x0F -> do
        a <- arbitrary
        return $ SBPlayer a

      0x10 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ SBVehicleMove a b c d e

      0x11 -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBSteerBoat a b

      0x12 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBPlayerAbilities a b c

      0x13 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBPlayerDigging a b c

      0x14 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBEntityAction a b c

      0x15 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBSteerVehicle a b c

      0x16 -> do
        a <- arbitrary
        return $ SBResourcePackStatus a

      0x17 -> do
        a <- arbitrary
        return $ SBHeldItemChange a

      0x18 -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBCreativeInventoryAction a b

      0x19 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ SBUpdateSign a b c d e

      0x1A -> do
        a <- arbitrary
        return $ SBAnimation a

      0x1B -> do
        a <- arbitrary
        return $ SBSpectate a

      0x1C -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ SBPlayerBlockPlacement a b c d e f

      0x1D -> do
        a <- arbitrary
        return $ SBUseItem a

prop_varIntEq :: [VarInt] -> Bool
prop_varIntEq [] = True
prop_varIntEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeVarInt x)) lst
  let decoded = fmap (parseOnly decodeVarInt) encoded :: [Either String VarInt]
  lst == (rights decoded)


prop_varLongEq :: [VarLong] -> Bool
prop_varLongEq [] = True
prop_varLongEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeVarLong x)) lst
  let decoded = fmap (parseOnly decodeVarLong) encoded :: [Either String VarLong]
  lst == (rights decoded)


prop_textEq :: [T.Text] -> Bool
prop_textEq [] = True
prop_textEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeText x)) lst
  let decoded = fmap (parseOnly decodeText) encoded :: [Either String T.Text]
  lst == (rights decoded)


prop_entityMetadataEq :: [EntityMetadata] -> Bool
prop_entityMetadataEq [] = True
prop_entityMetadataEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeEntityMetadata x)) lst
  let decoded = fmap (parseOnly decodeEntityMetadata) encoded :: [Either String EntityMetadata]
  lst == (rights decoded)


prop_slotEq :: [Slot] -> Bool
prop_slotEq [] = True
prop_slotEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeSlot x)) lst
  let decoded = fmap (parseOnly decodeSlot) encoded :: [Either String Slot]
  lst == (rights decoded)


prop_chunkSectionEq :: [ChunkSection] -> Bool
prop_chunkSectionEq [] = True
prop_chunkSectionEq lst = foldr (==) True $ (go lst)
  where
  go [] = []
  go (x:xs) =
    case x of
      OverworldChunkSection {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodeChunkSection x
        let decoded = parseOnly (decodeChunkSection True) encoded :: Either String ChunkSection
        (Right x == decoded):(go xs)
      OtherChunkSection {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodeChunkSection x
        let decoded = parseOnly (decodeChunkSection False) encoded :: Either String ChunkSection
        (Right x == decoded):(go xs)


prop_positionEq :: [Position] -> Bool
prop_positionEq [] = True
prop_positionEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodePosition x)) lst
  let decoded = fmap (parseOnly decodePosition) encoded :: [Either String Position]
  lst == (rights decoded)


prop_angleEq :: [Angle] -> Bool
prop_angleEq [] = True
prop_angleEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeAngle x)) lst
  let decoded = fmap (parseOnly decodeAngle) encoded :: [Either String Angle]
  lst == (rights decoded)


prop_uuidEq :: [UUID] -> Bool
prop_uuidEq [] = True
prop_uuidEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeUUID x)) lst
  let decoded = fmap (parseOnly decodeUUID) encoded :: [Either String UUID]
  lst == (rights decoded)


prop_byteStringEq :: [B.ByteString] -> Bool
prop_byteStringEq [] = True
prop_byteStringEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeByteString x)) lst
  let decoded = fmap (parseOnly decodeByteString) encoded :: [Either String B.ByteString]
  lst == (rights decoded)


prop_statisticEq :: [Statistic] -> Bool
prop_statisticEq [] = True
prop_statisticEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeStatistic x)) lst
  let decoded = fmap (parseOnly decodeStatistic) encoded :: [Either String Statistic]
  lst == (rights decoded)


prop_playerPropertyEq :: [PlayerProperty] -> Bool
prop_playerPropertyEq [] = True
prop_playerPropertyEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodePlayerProperty x)) lst
  let decoded = fmap (parseOnly decodePlayerProperty) encoded :: [Either String PlayerProperty]
  lst == (rights decoded)


prop_playerListActionEq :: [PlayerListAction] -> Bool
prop_playerListActionEq [] = True
prop_playerListActionEq lst = foldr (==) True (go lst)
  where
  go [] = []
  go (x:xs) =
    case x of
      PlayerListAdd {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListAction x
        let decoded = parseOnly (decodePlayerListAction 0) encoded :: Either String PlayerListAction
        (Right x == decoded):(go xs)
      PlayerListUpdateGameMode {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListAction x
        let decoded = parseOnly (decodePlayerListAction 1) encoded :: Either String PlayerListAction
        (Right x == decoded):(go xs)
      PlayerListUpdateLatency {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListAction x
        let decoded = parseOnly (decodePlayerListAction 2) encoded :: Either String PlayerListAction
        (Right x == decoded):(go xs)
      PlayerListUpdateDisplayName {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListAction x
        let decoded = parseOnly (decodePlayerListAction 3) encoded :: Either String PlayerListAction
        (Right x == decoded):(go xs)
      PlayerListRemovePlayer -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListAction x
        let decoded = parseOnly (decodePlayerListAction 4) encoded :: Either String PlayerListAction
        (Right x == decoded):(go xs)


prop_playerListEntryEq :: [PlayerListEntry] -> Bool
prop_playerListEntryEq [] = True
prop_playerListEntryEq lst = foldr (==) True (go lst)
  where
  go [] = []
  go (x:xs) =
    case playerListAction x of
      PlayerListAdd {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListEntry x
        let decoded = parseOnly (decodePlayerListEntry 0) encoded :: Either String PlayerListEntry
        (Right x == decoded):(go xs)
      PlayerListUpdateGameMode {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListEntry x
        let decoded = parseOnly (decodePlayerListEntry 1) encoded :: Either String PlayerListEntry
        (Right x == decoded):(go xs)
      PlayerListUpdateLatency {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListEntry x
        let decoded = parseOnly (decodePlayerListEntry 2) encoded :: Either String PlayerListEntry
        (Right x == decoded):(go xs)
      PlayerListUpdateDisplayName {} -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListEntry x
        let decoded = parseOnly (decodePlayerListEntry 3) encoded :: Either String PlayerListEntry
        (Right x == decoded):(go xs)
      PlayerListRemovePlayer -> do
        let encoded = BL.toStrict . BB.toLazyByteString $ encodePlayerListEntry x
        let decoded = parseOnly (decodePlayerListEntry 4) encoded :: Either String PlayerListEntry
        (Right x == decoded):(go xs)


prop_iconEq :: [Icon] -> Bool
prop_iconEq [] = True
prop_iconEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeIcon x)) lst
  let decoded = fmap (parseOnly decodeIcon) encoded :: [Either String Icon]
  lst == (rights decoded)


prop_entityPropertyEq :: [EntityProperty] -> Bool
prop_entityPropertyEq [] = True
prop_entityPropertyEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeEntityProperty x)) lst
  let decoded = fmap (parseOnly decodeEntityProperty) encoded :: [Either String EntityProperty]
  lst == (rights decoded)


prop_SBHandshakingEq :: [SBHandshaking] -> Bool
prop_SBHandshakingEq [] = True
prop_SBHandshakingEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeSBHandshaking x)) lst
  let decoded = fmap (parseOnly decodeSBHandshaking) encoded :: [Either String SBHandshaking]
  lst == (rights decoded)


prop_CBStatusEq :: [CBStatus] -> Bool
prop_CBStatusEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeCBStatus x)) lst
  let decoded = fmap (parseOnly decodeCBStatus) encoded :: [Either String CBStatus]
  lst == (rights decoded)


prop_SBStatusEq :: [SBStatus] -> Bool
prop_SBStatusEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeSBStatus x)) lst
  let decoded = fmap (parseOnly decodeSBStatus) encoded :: [Either String SBStatus]
  lst == (rights decoded)


prop_CBLoginEq :: [CBLogin] -> Bool
prop_CBLoginEq [] = True
prop_CBLoginEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeCBLogin x)) lst
  let decoded = fmap (parseOnly decodeCBLogin) encoded :: [Either String CBLogin]
  lst == (rights decoded)


prop_SBLoginEq :: [SBLogin] -> Bool
prop_SBLoginEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeSBLogin x)) lst
  let decoded = fmap (parseOnly decodeSBLogin) encoded :: [Either String SBLogin]
  lst == (rights decoded)


prop_CBPlayEq :: [CBPlay] -> Bool
prop_CBPlayEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeCBPlay x)) lst
  let decoded = fmap (parseOnly decodeCBPlay) encoded :: [Either String CBPlay]
  lst == (rights decoded)


prop_SBPlayEq :: [SBPlay] -> Bool
prop_SBPlayEq [] = True
prop_SBPlayEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeSBPlay x)) lst
  let decoded = fmap (parseOnly decodeSBPlay) encoded :: [Either String SBPlay]
  lst == (rights decoded)


main :: IO ()
main = hspec $ do

  describe "Minecraft Protocol Core Types" $ do
    context "VarInt:" $ do
      it "Identity" $ property prop_varIntEq
    context "VarLong:" $ do
      it "Identity" $ property prop_varLongEq
    context "String:" $ do
      it "Identity" $ property prop_textEq
    context "EntityMetadata:" $ do
      it "Identity" $ property prop_entityMetadataEq
    context "Slot:" $ do
      it "Identity" $ property prop_slotEq
    context "ChunkSection:" $ do
      it "Identity" $ property prop_chunkSectionEq
    context "Position:" $ do
      it "Identity" $ property prop_positionEq
    context "Angle:" $ do
      it "Identity" $ property prop_angleEq
    context "UUID:" $ do
      it "Identity" $ property prop_uuidEq
    context "ByteArray:" $ do
      it "Identity" $ property prop_byteStringEq

  describe "Minecraft Protocol Custom Records" $ do
    context "Statistic:" $ do
      it "Identity" $ property prop_statisticEq
    context "PlayerProperty:" $ do
      it "Identity" $ property prop_playerPropertyEq
    context "PlayerListAction:" $ do
      it "Identity" $ property prop_playerListActionEq
    context "PlayerListEntry:" $ do
      it "Identity" $ property prop_playerListEntryEq
    context "Icon:" $ do
      it "Identity" $ property prop_iconEq
    context "EntityProperty:" $ do
      it "Identity" $ property prop_entityPropertyEq

  describe "Minecraft Protocol Packets" $ do
    context "Server bound handshaking packets:" $ do
      it "Identity" $ property prop_SBHandshakingEq
    context "Client bound status packets:" $ do
      it "Identity" $ property prop_CBStatusEq
    context "Server bound status packets:" $ do
      it "Identity" $ property prop_SBStatusEq
    context "Client bound login packets:" $ do
      it "Identity" $ property prop_CBLoginEq
    context "Server bound login packets:" $ do
      it "Identity" $ property prop_SBLoginEq
    context "Client bound play packets:" $ do
      it "Identity" $ property prop_CBPlayEq
    context "Server bound play packets:" $ do
      it "Identity" $ property prop_SBPlayEq
