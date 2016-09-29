{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Handle.Play
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Handle.Play
  ( handlePlay
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.Attoparsec.Text as A
import Data.Conduit
import Data.Int
import Data.List
import qualified Data.Map.Strict as MS
import Data.NBT
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text.Encoding
import Data.UUID.V4
import OpenSandbox.Config
import OpenSandbox.Event
import OpenSandbox.Logger
import OpenSandbox.Protocol.Packet (CBPlay(..),SBPlay(..))
import OpenSandbox.Protocol.Types
import OpenSandbox.Time
import OpenSandbox.User
import OpenSandbox.World

logMsg :: Logger -> Lvl -> String -> IO ()
logMsg logger lvl msg = logIO logger "OpenSandbox.Protocol.Handle.Play" lvl (T.pack msg)

handlePlay  :: Config -> Logger -> WorldClock -> World -> TVar [Event] -> TVar UserStore -> Conduit SBPlay (StateT Session IO) CBPlay
handlePlay config logger worldClock world history userStore = do
  session <- lift $ State.get
  userStore <- liftIO $ readTVarIO userStore
  user <- case sessionUsername session of
            Nothing -> error "NO USERNAME FOUND IN SESSION!"
            Just username ->
              case MS.lookup username userStore of
                Nothing -> error "NO USER FOUND IN USERSTORE!"
                Just user -> return user
  serveGameState config user

  awaitForever $ \packet -> do
        liftIO $ threadDelay 10000
        liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show packet
        age <- liftIO $ getWorldAge worldClock
        t <- liftIO $ getWorldTime worldClock
        maybeOutgoing <- liftIO $ atomically $ (handle history age) packet
        forM_ maybeOutgoing yield
        -- Rewind
        when (mod t 1000 == 0) $ do
          past <- liftIO $ readTVarIO history
          forM_ past $ \event -> do
            liftIO $ logMsg logger LvlDebug $ "Undoing: " ++ show event
            liftIO $ threadDelay 10000
            yield . eventToCBPlay $ event
          liftIO $ atomically $ writeTVar history []
        when (mod t 20 == 0) $ sendAndLog $ CBTimeUpdate age t
        when (mod t 40 == 0) $ sendAndLog $ CBKeepAlive 5346


  where
    sendAndLog :: MonadIO m => CBPlay -> Conduit SBPlay m CBPlay
    sendAndLog packet = do
      liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show packet
      yield packet

    serveGameState :: MonadIO m => Config -> User -> Conduit SBPlay m CBPlay
    serveGameState config user = do
      sendAndLog $
        CBJoinGame
          2566
          (srvGameMode config)
          (srvDimension config)
          (srvDifficulty config)
          (srvMaxPlayers config)
          (T.pack . show $ srvWorldType config)
          True
      sendAndLog $ CBPluginMessage "MC|Brand" (encodeUtf8 "opensandbox")
      sendAndLog $ CBPluginMessage "REGISTER" (encodeUtf8 "MC|Brand")
      sendAndLog $ CBServerDifficulty (srvDifficulty config)
      sendAndLog $ CBSpawnPosition 0
      sendAndLog $ CBPlayerAbilities 0 1028443341 0
      sendAndLog $ CBHeldItemChange 0
      sendAndLog $ CBEntityStatus 32 AnimalInLove
      sendAndLog $ CBStatistics []
      sendAndLog $ CBPlayerListItem $
        PlayerListAdds
          [ PlayerListAdd (getUserUUID user) (getUserName user) [] Survival 0 Nothing
          ]
      sendAndLog $ CBPlayerPositionAndLook 0 4 0 0 0 0 777
      sendAndLog $ CBWorldBorder $
        Initialize 0 0 4723321873536909312 4723321873536909312 0 29999984 5 15
      worldAge <- liftIO $ getWorldAge worldClock
      worldTime <- liftIO $ getWorldTime worldClock
      sendAndLog $ CBTimeUpdate worldAge worldTime
      sendAndLog $ CBWindowItems 0 (V.replicate 46 (mkSlot (-1) 1 1 (NBT "" (ByteTag 0))))
      sendAndLog $ CBSetSlot (-1) (-1) (mkSlot (-1) 1 1 (NBT "" (ByteTag 0)))
      mapM_ (yield . CBChunkData) $ pullWorld world

    handle :: TVar [Event] -> Int64 -> SBPlay -> STM (Maybe CBPlay)
    handle eventJournal age packet =
      case packet of
        SBTeleportConfirm {} -> return Nothing
        SBTabComplete txt shouldAssumeCommand _ ->
          if shouldAssumeCommand
             then case A.parseOnly assumedCommand txt of
                    Left _ -> return (Just $ CBTabComplete V.empty)
                    Right prefix -> do
                      let matches = filter (T.isPrefixOf prefix) availableCommands
                      return (Just $ CBTabComplete (V.fromList matches))
             else case A.parseOnly potentialCommand txt of
                    Left _ -> return (Just $ CBTabComplete V.empty)
                    Right prefix -> do
                      let matches = filter (T.isPrefixOf prefix) availableCommands
                      return (Just $ CBTabComplete (V.fromList matches))

        SBChatMessage message -> do
          past <- readTVar eventJournal
          writeTVar eventJournal $ (ChatMessage age message 0):past
          return (Just $ CBChatMessage (Chat message) 0)
        SBClientStatus {} -> return Nothing
        SBClientSettings {} -> return Nothing
        SBConfirmTransaction {} -> return Nothing
        SBEnchantItem {} -> return Nothing
        SBClickWindow {} -> return Nothing
        SBCloseWindow {} -> return Nothing
        SBPluginMessage {} -> return Nothing
        SBUseEntity {} -> return Nothing
        SBKeepAlive {} -> return Nothing
        SBPlayerPosition x y z onGround -> do
          past <- readTVar eventJournal
          case find isLatestPlayerPositionAndLook past of
            Nothing ->
              writeTVar eventJournal [(PlayerPositionAndLook age x y z 0 0 True)]
            Just (PlayerPositionAndLook _  _ _ _ yaw pitch _) ->
              writeTVar eventJournal $ (PlayerPositionAndLook age x y z yaw pitch onGround):past
            Just _ -> undefined
          return Nothing

        SBPlayerPositionAndLook x y z yaw pitch onGround -> do
          past <- readTVar eventJournal
          writeTVar eventJournal $ (PlayerPositionAndLook age x y z yaw pitch onGround):past
          return Nothing

        SBPlayerLook yaw pitch onGround -> do
          past <- readTVar eventJournal
          case find isLatestPlayerPositionAndLook past of
            Nothing -> do
              writeTVar eventJournal [(PlayerPositionAndLook age 0 0 0 yaw pitch onGround)]
              return Nothing
            Just (PlayerPositionAndLook _ x0 y0 z0 _ _ _) -> do
              writeTVar eventJournal $ (PlayerPositionAndLook age x0 y0 z0 yaw pitch onGround):past
              return Nothing
            Just _ -> undefined

        SBPlayer {} -> return Nothing
        SBVehicleMove {} -> return Nothing
        SBSteerBoat {} -> return Nothing
        SBPlayerAbilities {} -> return Nothing
        SBPlayerDigging {} -> return Nothing
        SBEntityAction {} -> return Nothing
        SBSteerVehicle {} -> return Nothing
        SBResourcePackStatus {} -> return Nothing
        SBHeldItemChange {} -> return Nothing
        SBCreativeInventoryAction {} -> return Nothing
        SBUpdateSign {} -> return Nothing
        SBAnimation {} -> return Nothing
        SBSpectate {} -> return Nothing
        SBPlayerBlockPlacement {} -> return Nothing
        SBUseItem {} -> return Nothing
      where
        isLatestPlayerPositionAndLook PlayerPositionAndLook{} = True
        isLatestPlayerPositionAndLook _ = False

assumedCommand :: A.Parser T.Text
assumedCommand = do
  A.skip (=='/')
  A.takeText

potentialCommand :: A.Parser T.Text
potentialCommand = do
  _ <- A.char '/'
  txt <- A.takeText
  return $ T.cons '/' txt

availableCommands :: [T.Text]
availableCommands = ["/rewind","/help"]

eventToCBPlay :: Event -> CBPlay
eventToCBPlay (PlayerPositionAndLook age x y z yaw pitch _) =
  CBPlayerPositionAndLook x y z yaw pitch 0 (fromEnum age)
eventToCBPlay (ChatMessage _ message position) =
  CBChatMessage (Chat message) position

