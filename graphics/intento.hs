import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Graphics.UI.GLUT.Window

import Data.Word
import Foreign
import Graphics.Rendering.OpenGL

import Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage

import Data.Bits

myHeight = 220
myWidth = 220
myWidth2 = 220
myHeight2 = 220

readPixelArray :: Int -> Int -> Int -> Int -> IO [Word32]
readPixelArray x y w h = do
    let arraySize = w * h
    array <- mallocForeignPtrArray arraySize :: IO (ForeignPtr Word32)
    withForeignPtr array $ \ptr -> do
        -- ptr :: Ptr Word32
        -- fromIntegral is needed because Position and Size store GLints not Ints
        let position = Position (fromIntegral x) (fromIntegral y)
        let size = Size (fromIntegral w) (fromIntegral h)
        readPixels position size $ PixelData RGBA UnsignedByte ptr
        peekArray arraySize ptr



circle (x, y) radius divs = map toPoint angles where 
    arc       = 2*pi / fromIntegral divs
    toPoint a = (x + cos a * radius, y + sin a * radius)
    angles    = map ((*arc) . fromIntegral) [0..divs]

display :: IO ()
display = do
  clear [ ColorBuffer ]

  renderPrimitive Polygon $ do
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) (circle (0,0) (1) 30 :: [(GLfloat, GLfloat)])
  
  swapBuffers -- esto hace flush y otras cosas
  a <- readPixelArray (0) (0) myWidth2 myHeight2 ----------------------------------------
  writeFile "output" $ reverse $ fst $ foldl foldealo ("",1) a
  where
    foldealo = (\(s,cnt) x -> if cnt==myWidth then (('\n'):((f x):s),1) else ((f x):s,cnt+1))
    f 0 = '0'
    f _ = '1'

main :: IO ()
main = do
  -- Initialize OpenGL via GLUT
  (progname, _) <- getArgsAndInitialize
  initialWindowSize $= Size myWidth myHeight -----------------------------------------
  initialWindowPosition $= Position 700 100
  initialDisplayMode $= [DoubleBuffered,RGBAMode]
  createWindow progname
  drawBuffer $= FrontAndBackBuffers
  displayCallback $= display
  mainLoop
