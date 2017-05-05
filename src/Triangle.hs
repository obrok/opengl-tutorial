module Triangle (main) where

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

main :: IO ()
main = do
  window <- Window.init
  vao <- makeVAO
  makeBuffer noVertices
  mainLoop window vao

mainLoop :: GLFW.Window -> GL.VertexArrayObject -> IO ()
mainLoop window vao = do
  render vao
  GLFW.swapBuffers window
  GLFW.pollEvents
  escape <- GLFW.getKey window GLFW.Key'Escape
  if escape == GLFW.KeyState'Pressed
    then GLFW.setWindowShouldClose window True
    else mainLoop window vao

render :: GL.VertexArrayObject -> IO ()
render vao = do
  GLUtil.withVAO vao $ VertexArrays.drawArrays PrimitiveMode.Triangles 0 3
  GLError.throwError

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

makeVAO :: IO GL.VertexArrayObject
makeVAO = GLUtil.makeVAO $ do
  VertexArrays.vertexAttribArray attribLocation $= VertexArrays.Enabled
  -- The below is kinda dumb... The buffer is not part of what is remembered by the VAO...
  makeBuffer vertices
  -- Important! Must set these after the buffer is bound, otherwise it doesn't take!
  -- AHA! The buffer from this point is remembered, even though some other buffer is bound later.
  -- That's at least true for drawArrays.
  VertexArrays.vertexAttribPointer attribLocation $=
    (VertexSpec.ToFloat, VertexArrays.VertexArrayDescriptor 3 VertexArrays.Float 0 Ptr.nullPtr)
  shader <- loadShaderProgram
  -- The program is also set globally
  GL.currentProgram $= Just (GLUtil.program shader)

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

loadShaderProgram :: IO GLUtil.ShaderProgram
loadShaderProgram = do
  vertexPath <- Paths.getDataFileName "src/shaders/red.vert"
  fragmentPath <- Paths.getDataFileName "src/shaders/red.frag"
  GLUtil.simpleShaderProgram vertexPath fragmentPath
