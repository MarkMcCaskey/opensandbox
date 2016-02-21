{-# LANGUAGE OverloadedStrings #-}
import            Control.Monad
import            Data.Array.Repa as Repa
import qualified  Data.Text as T
import            Data.Word
import            Graphics.GL.Compatibility32
import            Graphics.GL.Core45
import            Graphics.GL.Types
import            Linear.V3
import            Linear.V4
import            SDL

data Chunk = Chunk
  { blk       :: !(Array U DIM3 Word8)
  , vbo       :: !GLuint
  , elements  :: !Int
  , changed   :: !Bool
  } deriving (Show,Eq)

--initChunk :: IO Chunk

--get :: Array U DIM3 Word8 -> V3 Int -> IO Word8

--set :: Array U DIM3 Word8 -> V3 Int -> Word8 -> IO ()

--update :: IO ()

--render :: IO ()

gVertexBufferData :: [Float]
gVertexBufferData = [ (-1.0),(-1.0),(-1.0)
                    , (-1.0),(-1.0),(1.0)
                    , (-1.0),(1.0),(1.0)
                    , (1.0),(1.0),(-1.0)
                    , (-1.0),(-1.0),(-1.0)
                    , (-1.0),(1.0),(-1.0)
                    , (1.0),(-1.0),(1.0)
                    , (-1.0),(-1.0),(-1.0)
                    , (1.0),(-1.0),(-1.0)
                    , (1.0),(1.0),(-1.0)
                    , (1.0),(-1.0),(-1.0)
                    , (-1.0),(-1.0),(-1.0)
                    , (-1.0),(-1.0),(-1.0)
                    , (-1.0),(1.0),(1.0)
                    , (-1.0),(1.0),(-1.0)
                    , (1.0),(-1.0),(1.0)
                    , (-1.0),(-1.0),(1.0)
                    , (-1.0),(-1.0),(-1.0)
                    , (-1.0),(1.0),(1.0)
                    , (-1.0),(-1.0),(1.0)
                    , (1.0),(-1.0),(1.0)
                    , (1.0),(1.0),(1.0)
                    , (1.0),(-1.0),(-1.0)
                    , (1.0),(1.0),(-1.0)
                    , (1.0),(-1.0),(-1.0)
                    , (1.0),(1.0),(1.0)
                    , (1.0),(-1.0),(1.0)
                    , (1.0),(1.0),(1.0)
                    , (1.0),(1.0),(-1.0)
                    , (-1.0),(1.0),(-1.0)
                    , (1.0),(1.0),(1.0)
                    , (-1.0),(1.0),(-1.0)
                    , (-1.0),(1.0),(1.0)
                    , (1.0),(1.0),(1.0)
                    , (-1.0),(1.0),(1.0)
                    , (1.0),(-1.0),(1.0)]


initGL :: IO ()
initGL = do
  glClearColor 0 0 0 1
  glClearDepth 1
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL
  glShadeModel GL_SMOOTH
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST

draw :: IO ()
draw = do
  glClear GL_COLOR_BUFFER_BIT
  glClear GL_DEPTH_BUFFER_BIT
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  glTranslatef (1.5) (0.0) (-7.0)

  glBegin GL_QUADS
  glColor3f 0 0 0
  glVertex3f (1.0) (1.0) (-1.0)
  glVertex3f (-1.0) (1.0) (-1.0)
  glVertex3f (-1.0) (1.0) (1.0)
  glVertex3f (1.0) (1.0) (1.0)

  glColor3f (1.0) (0.5) (0.0)
  glVertex3f (1.0) (-1.0) (1.0)
  glVertex3f (-1.0) (-1.0)  (1.0)
  glVertex3f (-1.0) (-1.0) (-1.0)
  glVertex3f (1.0) (-1.0) (-1.0)

  glColor3f (1.0) (0.0) (0.0)
  glVertex3f (1.0) (1.0) (1.0)
  glVertex3f (-1.0) (1.0) (1.0)
  glVertex3f (-1.0) (-1.0) (1.0)
  glVertex3f ( 1.0) (-1.0) (1.0)

  glColor3f (1.0) (1.0) (0.0)
  glVertex3f (1.0) (-1.0) (-1.0)
  glVertex3f (-1.0) (-1.0) (-1.0)
  glVertex3f (-1.0) (1.0) (-1.0)
  glVertex3f (1.0) (1.0) (-1.0)

  glColor3f (0.0) (0.0) (1.0)
  glVertex3f (-1.0) (1.0) (1.0)
  glVertex3f (-1.0) (1.0) (-1.0)
  glVertex3f (-1.0) (-1.0) (-1.0)
  glVertex3f (-1.0) (-1.0)  (1.0)

  glColor3f (1.0) (0.0) (1.0)
  glVertex3f (1.0) (1.0) (-1.0)
  glVertex3f (1.0) (1.0) (1.0)
  glVertex3f (1.0) (-1.0) (1.0)
  glVertex3f (1.0) (-1.0) (-1.0)
  glEnd
  glLoadIdentity
  glTranslatef (-1.5) (0.0) (-6.0)

  glBegin GL_TRIANGLES
  glColor3f (1.0) (0.0) (0.0)
  glVertex3f (0.0) (1.0) (0.0)
  glColor3f (0.0) (1.0) (0.0)
  glVertex3f (-1.0) (-1.0) (1.0)
  glColor3f (0.0) (0.0) (1.0)
  glVertex3f (1.0) (-1.0) (1.0)

  glColor3f (1.0) (0.0) (0.0)
  glVertex3f (0.0) (1.0) (0.0)
  glColor3f (0.0) (0.0) (1.0)
  glVertex3f (1.0) (-1.0) (1.0)
  glColor3f (0.0) (1.0) (0.0)
  glVertex3f (1.0) (-1.0) (-1.0)

  glColor3f (1.0) (0.0) (0.0)
  glVertex3f (0.0) (1.0) (0.0)
  glColor3f (0.0) (1.0) (0.0)
  glVertex3f (1.0) (-1.0) (-1.0)
  glColor3f (0.0) (0.0) (1.0)
  glVertex3f (-1.0) (-1.0) (-1.0)

  glColor3f (1.0) (0.0) (0.0)
  glVertex3f (0.0) (1.0) (0.0)
  glColor3f (0.0) (0.0) (1.0)
  glVertex3f (-1.0) (-1.0) (-1.0)
  glColor3f (0.0) (1.0) (0.0)
  glVertex3f (-1.0) (-1.0) (1.0)
  glEnd

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
