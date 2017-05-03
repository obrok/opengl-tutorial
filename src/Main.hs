import Graphics.UI.GLFW as GLFW
import Graphics.Window as Window

main :: IO ()
main = do
  window <- Window.init
  mainLoop window

mainLoop :: Window -> IO ()
mainLoop window = do
  GLFW.swapBuffers window
  GLFW.pollEvents
  escape <- GLFW.getKey window Key'Escape
  if escape == KeyState'Pressed
    then setWindowShouldClose window True
    else mainLoop window
