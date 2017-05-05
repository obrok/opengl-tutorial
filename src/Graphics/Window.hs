{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Window (Graphics.Window.init) where

import Graphics.UI.GLFW as GLFW

aaSamples :: Int
aaSamples = 4

openGLMajor :: Int
openGLMajor = 3

openGLMinor :: Int
openGLMinor = 3

resX :: Int
resX = 1024

resY :: Int
resY = 768

windowTitle :: String
windowTitle = "Tutorial"

init :: IO Window
init = do
  True <- GLFW.init

  GLFW.windowHint (WindowHint'Samples aaSamples)
  GLFW.windowHint (WindowHint'ContextVersionMajor openGLMajor)
  GLFW.windowHint (WindowHint'ContextVersionMinor openGLMinor)
  GLFW.windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  GLFW.windowHint (WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (WindowHint'ClientAPI ClientAPI'OpenGL)

  Just window <- GLFW.createWindow resX resY windowTitle Nothing Nothing

  GLFW.makeContextCurrent (Just window)

  return window
