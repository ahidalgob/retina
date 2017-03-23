import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Graphics.UI.GLUT.Window

import Data.Word
import Foreign
import Graphics.Rendering.OpenGL

import Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage

import Data.Bits



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


data Pixel = Pixel {
    red :: Word8, green :: Word8, blue :: Word8, alpha :: Word8
} deriving (Eq, Show)

readPixels' :: Int -> Int -> Int -> Int -> IO [Pixel]
readPixels' x y w h = do
    rawPixels <- readPixelArray x y w h
    return $ map pixelFromWord32 rawPixels

-- pixelFromWord32 0xAABBCCDD = Pixel 0xAA 0xBB 0xCC 0xDD
pixelFromWord32 :: Word32 -> Pixel
pixelFromWord32 colour = Pixel (extract 3) (extract 2) (extract 1) (extract 0)
    where extract idx = fromIntegral $ (colour `shiftR` (idx * 8)) .&. 0xFF



myinit :: IO ()
myinit = do
    drawBuffer $= FrontAndBackBuffers
    readBuffer $= FrontAndBackBuffers
    -- glClearColor(0.0, 0.0, 0.0, 0.0);
    rowAlignment Unpack $= 1
    viewport $= (Position 0 0, Size 100 100)
    matrixMode $= Projection
    loadIdentity
    matrixMode $= Modelview 4

main :: IO ()
main = do
  -- Initialize OpenGL via GLUT
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,RGBAMode]
  initialWindowSize $= Size 100 100
  initialWindowPosition $= Position 700 100

  myinit

  -- Create the output window
  createWindow progname
-- Every time the window needs to be updated, call the display function
  displayCallback $= display
  
  -- Let GLUT handle the window events, calling the displayCallback as fast as it can
  mainLoop



circle (x, y) radius divs = map toPoint angles where 
    arc       = (pi/2) / fromIntegral divs
    toPoint a = (x + cos a * radius, y + sin a * radius)
    angles    = map ((*arc) . fromIntegral) [0..divs]


display :: IO ()
display = do
  -- Clear the screen with the default clear color (black)
  clear [ ColorBuffer ]
  loadIdentity
  let points = circle (0,0) (1) 5 :: [(GLfloat, GLfloat)]

  renderPrimitive Polygon $ do
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) points


  swapBuffers
  a <- readPixels' (0) (0) 100 100
  mapM_ print a

  -- Send all of the drawing commands to the OpenGL server
  --flush

