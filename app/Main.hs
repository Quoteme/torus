module Main where

import qualified Torus as T
import OrbitPointOfView
import Graphics.UI.GLUT
import Data.Foldable
import Data.IORef

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer]
  _window <- createWindow "Torus"
  depthFunc $= Just Less
  --
  lighting $= Enabled
  position (Light 0) $= Vertex4 0 2 4 1
  ambient (Light 0) $= Color4 1 1 1 1
  light (Light 0) $= Enabled
  --
  pPos <- newIORef (0::Int,0::Int,2.0)
  keyboardMouseCallback $= Just (keyboard pPos)
  --
  displayCallback $= display pPos
  reshapeCallback $= Just reshape
  mainLoop
  where
    keyboard pPos c _  _ _ = keyForPos pPos c

display pPos = do
  loadIdentity
  setPointOfView pPos
  clear [ ColorBuffer,DepthBuffer ]
  lighting $= Disabled
  currentColor $= Color4 1 0.3 0.3 1
  renderPrimitive LineStrip $
    mapM_ (\(T.Vector3D x y z) -> vertex $ Vertex3 x y z) torusLine
  lighting $= Enabled
  currentColor $= Color4 0 0 1 0
  renderPrimitive Quads $
    mapM_ (\(T.Vector3D x y z) -> vertex $ Vertex3 x y z) torusQuads
  -- swapBuffers
  flush
    where
      torusLine :: [T.Vector3D Float]
      torusLine = [T.toTorusCoordinates (T.Torus 0.75 0.25) (T.Vector2D x (4*x)) | x<-[0,step..1] ]
        where
          step = 0.01
      torusQuads :: [T.Vector3D Float]
      torusQuads = concat [ [ T.toTorusCoordinates (T.Torus 0.75 0.20) (T.Vector2D x y)
                            , T.toTorusCoordinates (T.Torus 0.75 0.20) (T.Vector2D x (y+step))
                            , T.toTorusCoordinates (T.Torus 0.75 0.20) (T.Vector2D (x+step) (y+step))
                            , T.toTorusCoordinates (T.Torus 0.75 0.20) (T.Vector2D (x+step) y) ]
                          | x<-[0,step..1], y<-[0,step..1] ]
        where
          step = 0.04
