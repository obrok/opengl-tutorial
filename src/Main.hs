import qualified System.Environment as Environment
import qualified Triangle
import qualified Cube

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    ["triangle"] -> Triangle.mainNoCamera
    ["triangle_camera"] -> Triangle.mainCamera
    ["cube"] -> Cube.main
    _ -> return ()
