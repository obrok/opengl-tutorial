module Triangle (mainCamera, mainNoCamera) where

import qualified Graphics.Window as Window
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Foreign.Storable as Storable
import qualified Foreign.Ptr as Ptr
import qualified Data.Array.Storable as StorableArray
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as VertexArrays
import qualified Graphics.Rendering.OpenGL.GL.VertexSpec as VertexSpec
import qualified Graphics.Rendering.OpenGL.GL.PrimitiveMode as PrimitiveMode
import qualified Graphics.GLUtil as GLUtil
import qualified Paths_opengl_tutorial as Paths
import qualified Graphics.GLUtil.GLError as GLError
import qualified Linear
import Linear ((!*!))

mainNoCamera :: IO ()
mainNoCamera = main "red" render

mainCamera :: IO ()
mainCamera = main "camera" renderCamera

main :: String -> (GL.VertexArrayObject -> GLUtil.ShaderProgram -> IO ()) -> IO ()
main shaderName action = do
  window <- Window.init
  (vao, shader) <- makeVAO shaderName
  makeBuffer noVertices
  mainLoop window (action vao shader)

mainLoop :: GLFW.Window -> IO () -> IO ()
mainLoop window renderAction = do
  renderAction
  GLFW.swapBuffers window
  GLFW.pollEvents
  escape <- GLFW.getKey window GLFW.Key'Escape
  if escape == GLFW.KeyState'Pressed
    then GLFW.setWindowShouldClose window True
    else mainLoop window renderAction

render :: GL.VertexArrayObject -> GLUtil.ShaderProgram -> IO ()
render vao _ = do
  GLUtil.withVAO vao $ VertexArrays.drawArrays PrimitiveMode.Triangles 0 3
  GLError.throwError

renderCamera :: GL.VertexArrayObject -> GLUtil.ShaderProgram -> IO ()
renderCamera vao shader = do
  GLUtil.withVAO vao $ do
    GLUtil.setUniform shader "MVP" mvpMatrix
    VertexArrays.drawArrays PrimitiveMode.Triangles 0 3
  GLError.throwError

mvpMatrix :: Linear.M44 GL.GLfloat
mvpMatrix = projectionMatrix !*! viewMatrix !*! modelMatrix

projectionMatrix :: Linear.M44 GL.GLfloat
projectionMatrix = Linear.perspective (pi / 4) (4 / 3) 0.1 100

viewMatrix :: Linear.M44 GL.GLfloat
viewMatrix = Linear.lookAt eye center up
  where
    eye = Linear.V3 4 3 3
    center = Linear.V3 0 0 0
    up = Linear.V3 0 1 0

modelMatrix :: Linear.M44 GL.GLfloat
modelMatrix = Linear.identity

vertices :: [GL.GLfloat]
vertices =
  [
    -1, -1, 0,
    1, -1, 0,
    0, 1, 0
 ]

noVertices :: [GL.GLfloat]
noVertices = []

attribLocation :: VertexSpec.AttribLocation
attribLocation = VertexSpec.AttribLocation 0

makeVAO :: String -> IO (GL.VertexArrayObject, GLUtil.ShaderProgram)
makeVAO shaderName = do
  vao <- GLUtil.makeVAO $ do
    VertexArrays.vertexAttribArray attribLocation $= VertexArrays.Enabled
    -- The below is kinda dumb... The buffer is not part of what is remembered by the VAO...
    makeBuffer vertices
    -- Important! Must set these after the buffer is bound, otherwise it doesn't take!
    -- AHA! The buffer from this point is remembered, even though some other buffer is bound later.
    -- That's at least true for drawArrays.
    VertexArrays.vertexAttribPointer attribLocation $=
      (VertexSpec.ToFloat, VertexArrays.VertexArrayDescriptor 3 VertexArrays.Float 0 Ptr.nullPtr)

  shader <- GLUtil.withVAO vao $ do
    -- The program is also set globally
    shader <- loadShaderProgram shaderName
    GL.currentProgram $= Just (GLUtil.program shader)
    return shader

  return (vao, shader)

makeBuffer :: Storable.Storable a => [a] -> IO ()
makeBuffer xs =
  let
    len = length xs
    n = fromIntegral $ bufferSize xs
    target = GL.ArrayBuffer
  in do
    [buffer] <- GL.genObjectNames 1
    GL.bindBuffer target $= Just buffer
    arr <- StorableArray.newListArray (0, len - 1) xs
    StorableArray.withStorableArray arr $ \ptr ->
      GL.bufferData target $= (n, ptr, GL.StaticDraw)

bufferSize :: forall a. Storable.Storable a => [a] -> Int
bufferSize [] = 0
bufferSize xs @ (x:_) = length xs * Storable.sizeOf x

loadShaderProgram :: String -> IO GLUtil.ShaderProgram
loadShaderProgram name = do
  vertexPath <- Paths.getDataFileName $ "src/shaders/" ++ name ++ ".vert"
  fragmentPath <- Paths.getDataFileName $ "src/shaders/" ++ name ++ ".frag"
  GLUtil.simpleShaderProgram vertexPath fragmentPath
