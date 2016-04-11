-- Triangle Tutorial adapted from Linus Arver's version on his blog
-- http://funloop.org/post/2014-03-15-opengl-from-haskell.html

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import            Control.Concurrent
import            Control.Monad
import            Foreign
import            Foreign.C.String
import            Foreign.C.Types
import            Graphics.GL.Compatibility32
import            Graphics.GL.Types
import            Linear
import            SDL

data GLIDs = GLIDs
  { progId          :: !GLuint
  , vertexArrayId   :: !GLuint
  , vertexBufferId  :: !GLuint
  }

vertexShader :: String
vertexShader = unlines
  [ "#version 330 core"
  , "layout (location = 0) in vec3 position;"
  , "void main()"
  , "{"
  , "gl_Position = vec4(position.x, position.y, position.z, 1.0);"
  , "}"
  ]

fragmentShader :: String
fragmentShader = unlines
  [ "#version 330 core"
  , "out vec4 color;"
  , "void main()"
  , "{"
  , "color = vec4(1.0f,0.5f,0.2f,1.0f);"
  , "}"
  ]

vertexBufferData :: [GLfloat]
vertexBufferData =
  [ (-0.5),(-0.5),(0.0)
  , (0.5),(-0.5),(0.0)
  , (0.0),(0.5),(0.0)
  ]

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)


initGL :: IO GLIDs
initGL = do
  glClearColor 0 0 0 1
  progId <- loadProgram vertexShader fragmentShader
  vaId <- newVAO
  bufId <- fillNewBuffer vertexBufferData
  return $ GLIDs
    { progId = progId
    , vertexArrayId = vaId
    , vertexBufferId = bufId
    }


freeResources :: GLIDs -> IO ()
freeResources GLIDs{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with vertexArrayId $ glDeleteVertexArrays 1


newVAO :: IO GLuint
newVAO = do
  vaId <- withNewPtr (glGenVertexArrays 1)
  glBindVertexArray vaId
  return vaId


fillNewBuffer :: [GLfloat] -> IO GLuint
fillNewBuffer xs = do
  bufId <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ARRAY_BUFFER bufId
  withArrayLen xs func
  return bufId
  where
  func len ptr = glBufferData
    GL_ARRAY_BUFFER
    (fromIntegral (len * sizeOf (undefined :: GLfloat)))
    (castPtr ptr :: Ptr ())
    GL_STATIC_DRAW


bindBufferToAttrib :: GLuint -> GLuint -> IO ()
bindBufferToAttrib bufId attribLoc = do
  glEnableVertexAttribArray attribLoc
  glBindBuffer GL_ARRAY_BUFFER bufId
  glVertexAttribPointer
    attribLoc
    3
    GL_FLOAT
    (fromBool False)
    0
    nullPtr


loadProgram :: String -> String -> IO GLuint
loadProgram vertShader fragShader = do
  shaderIds <- mapM (uncurry loadShader)
    [ (GL_VERTEX_SHADER, vertShader)
    , (GL_FRAGMENT_SHADER, fragShader)
    ]
  progId <- glCreateProgram
  putStrLn "Linking program"
  mapM_ (glAttachShader progId) shaderIds
  glLinkProgram progId
  _ <- checkStatus
      GL_LINK_STATUS glGetProgramiv glGetProgramInfoLog progId
  mapM_ glDeleteShader shaderIds
  return progId


loadShader :: GLenum -> String -> IO GLuint
loadShader shaderTypeFlag code = do
  shaderId <- glCreateShader shaderTypeFlag
  withCString code $ \codePtr ->
    with codePtr $ \codePtrPtr ->
      glShaderSource shaderId 1 codePtrPtr nullPtr
  putStrLn "Compiling shader..."
  glCompileShader shaderId
  _ <- checkStatus
      GL_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog shaderId
  return shaderId


checkStatus :: (Integral a1, Storable a1) => GLenum -> (t -> GLenum -> Ptr a1 -> IO a) -> (t -> a1 -> Ptr a3 -> Ptr Foreign.C.Types.CChar -> IO a2) -> t -> IO Bool
checkStatus statusFlag glGetFn glInfoLogFn componentId = do
  let fetch info = withNewPtr (glGetFn componentId info)
  status <- liftM toBool $ fetch statusFlag
  logLength <- fetch GL_INFO_LOG_LENGTH
  when (logLength > 0) $
    allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
      _ <- glInfoLogFn componentId logLength nullPtr msgPtr
      msg <- peekCString msgPtr
      (if status then putStrLn else fail) msg
  return status


draw :: GLIDs -> IO ()
draw GLIDs{..} = do
  glClearColor 0.5 0.7 1.0 1.0
  glClear GL_COLOR_BUFFER_BIT
  glClear GL_DEPTH_BUFFER_BIT
  glUseProgram progId
  bindBufferToAttrib vertexBufferId 0
  glDrawArrays GL_TRIANGLES 0 3
  glDisableVertexAttribArray 0


gameLoop :: Window -> GLIDs -> IO ()
gameLoop win glids = do
    events <- pollEvents
    let eventIsQPress event =
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            _ -> False
        qPressed = not (null (filter eventIsQPress events))
    draw glids
    glSwapWindow win
    threadDelay 10000
    unless qPressed (gameLoop win glids)


main :: IO ()
main = do
  initializeAll
  win <- createWindow "OpenSandbox" $ WindowConfig
    { windowBorder = True
    , windowHighDPI = False
    , windowInputGrabbed = False
    , windowMode = Windowed
    , windowOpenGL = Just $ OpenGLConfig
        { glColorPrecision = V4 8 8 8 0
        , glDepthPrecision = 24
        , glStencilPrecision = 8
        , glProfile = Core Normal 3 3
        }
    , windowPosition = Wherever
    , windowResizable = False
    , windowInitialSize = V2 800 600
    }
  context <- glCreateContext win
  glids <- initGL
  gameLoop win glids
  freeResources glids
  glDeleteContext context
  return ()
