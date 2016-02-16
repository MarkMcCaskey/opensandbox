{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- File         : client.hs
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
import            Control.Monad
import qualified  Data.Text as T
import            Linear.V3
import            Linear.V4
import            SDL


ticksPerSecond :: Int
ticksPerSecond = 60


sectorSize :: Int
sectorSize = 16


walkingSpeed :: Int
walkingSpeed = 5


flyingSpeed :: Int
flyingSpeed = 15


gravity :: Double
gravity = 20.0


texturePath :: FilePath
texturePath = "texture.png"


faces :: [V3 Int]
faces = [ V3  0  1  0
        , V3  0 (-1)  0
        , V3 (-1)  0  0
        , V3  1  0  0
        , V3  0  0  1
        , V3  0  0 (-1) ]


gameLoop :: Renderer -> IO ()
gameLoop renderer = do
    events <- pollEvents
    let eventIsQPress event =
          case eventPayload event of
            KeyboardEvent keyboardEvent -> 
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            _ -> False
        qPressed = not (null (filter eventIsQPress events))
    rendererDrawColor renderer $= V4 128 176 255 255
    clear renderer
    present renderer
    unless qPressed (gameLoop renderer)
                

main :: IO ()
main = do
    initializeAll
    window <- createWindow "OpenSandbox" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    gameLoop renderer
