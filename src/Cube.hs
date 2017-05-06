module Cube (main) where

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

main :: IO ()
main = do
  window <- Window.init
  GL.depthFunc $= Just GL.Less
  (vao, shader) <- makeVAO "cube"
  mainLoop window (renderCamera vao shader)

mainLoop :: GLFW.Window -> IO () -> IO ()
mainLoop window renderAction = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  renderAction
  GLFW.swapBuffers window
  GLFW.pollEvents
  escape <- GLFW.getKey window GLFW.Key'Escape
  if escape == GLFW.KeyState'Pressed
    then GLFW.setWindowShouldClose window True
    else mainLoop window renderAction

renderCamera :: GL.VertexArrayObject -> GLUtil.ShaderProgram -> IO ()
renderCamera vao shader = do
  GLUtil.withVAO vao $ do
    GLUtil.setUniform shader "MVP" mvpMatrix
    VertexArrays.drawArrays PrimitiveMode.Triangles 0 (3 * 12)
  GLError.throwError

mvpMatrix :: Linear.M44 GL.GLfloat
mvpMatrix = projectionMatrix !*! viewMatrix !*! modelMatrix

projectionMatrix :: Linear.M44 GL.GLfloat
projectionMatrix = Linear.perspective (pi / 2) (4 / 3) 0.1 100

viewMatrix :: Linear.M44 GL.GLfloat
viewMatrix = Linear.lookAt eye center up
  where
    eye = Linear.V3 4 3 (-3)
    center = Linear.V3 0 0 0
    up = Linear.V3 0 1 0

modelMatrix :: Linear.M44 GL.GLfloat
modelMatrix = Linear.mkTransformationMat rotation translation
  where
    rotation = Linear.identity
    translation = Linear.V3 3 0 0

vertices :: [GL.GLfloat]
vertices =
  [
    -1,-1,-1,
    -1,-1, 1,
    -1, 1, 1,
    1, 1,-1,
    -1,-1,-1,
    -1, 1,-1,
    1,-1, 1,
    -1,-1,-1,
    1,-1,-1,
    1, 1,-1,
    1,-1,-1,
    -1,-1,-1,
    -1,-1,-1,
    -1, 1, 1,
    -1, 1,-1,
    1,-1, 1,
    -1,-1, 1,
    -1,-1,-1,
    -1, 1, 1,
    -1,-1, 1,
    1,-1, 1,
    1, 1, 1,
    1,-1,-1,
    1, 1,-1,
    1,-1,-1,
    1, 1, 1,
    1,-1, 1,
    1, 1, 1,
    1, 1,-1,
    -1, 1,-1,
    1, 1, 1,
    -1, 1,-1,
    -1, 1, 1,
    1, 1, 1,
    -1, 1, 1,
    1,-1, 1
 ]

colors :: [GL.GLfloat]
colors = [
    0.583,  0.771,  0.014,
    0.609,  0.115,  0.436,
    0.327,  0.483,  0.844,
    0.822,  0.569,  0.201,
    0.435,  0.602,  0.223,
    0.310,  0.747,  0.185,
    0.597,  0.770,  0.761,
    0.559,  0.436,  0.730,
    0.359,  0.583,  0.152,
    0.483,  0.596,  0.789,
    0.559,  0.861,  0.639,
    0.195,  0.548,  0.859,
    0.014,  0.184,  0.576,
    0.771,  0.328,  0.970,
    0.406,  0.615,  0.116,
    0.676,  0.977,  0.133,
    0.971,  0.572,  0.833,
    0.140,  0.616,  0.489,
    0.997,  0.513,  0.064,
    0.945,  0.719,  0.592,
    0.543,  0.021,  0.978,
    0.279,  0.317,  0.505,
    0.167,  0.620,  0.077,
    0.347,  0.857,  0.137,
    0.055,  0.953,  0.042,
    0.714,  0.505,  0.345,
    0.783,  0.290,  0.734,
    0.722,  0.645,  0.174,
    0.302,  0.455,  0.848,
    0.225,  0.587,  0.040,
    0.517,  0.713,  0.338,
    0.053,  0.959,  0.120,
    0.393,  0.621,  0.362,
    0.673,  0.211,  0.457,
    0.820,  0.883,  0.371,
    0.982,  0.099,  0.879
  ]

makeVAO :: String -> IO (GL.VertexArrayObject, GLUtil.ShaderProgram)
makeVAO shaderName = do
  vao <- GLUtil.makeVAO $ do
    VertexArrays.vertexAttribArray (GL.AttribLocation 0) $= VertexArrays.Enabled
    makeBuffer vertices
    VertexArrays.vertexAttribPointer (GL.AttribLocation 0) $=
      (VertexSpec.ToFloat, VertexArrays.VertexArrayDescriptor 3 VertexArrays.Float 0 Ptr.nullPtr)

    VertexArrays.vertexAttribArray (GL.AttribLocation 1) $= VertexArrays.Enabled
    makeBuffer colors
    VertexArrays.vertexAttribPointer (GL.AttribLocation 1) $=
      (VertexSpec.ToFloat, VertexArrays.VertexArrayDescriptor 3 VertexArrays.Float 0 Ptr.nullPtr)

  shader <- GLUtil.withVAO vao $ do
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
