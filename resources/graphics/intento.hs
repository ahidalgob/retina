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

dibujitoCirculo :: IO ()
dibujitoCirculo = do
  renderPrimitive Polygon $ do
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) (circle (0,0) (1) 30 :: [(GLfloat, GLfloat)])

dibujarRayitas :: IO ()
dibujarRayitas = do
  renderPrimitive Lines $ do
    vertex $ (Vertex2 (-1) (-1) :: Vertex2 GLfloat)
    vertex $ (Vertex2 1 1 :: Vertex2 GLfloat)
  renderPrimitive Lines $ do
    vertex $ (Vertex2 (-1) 1 :: Vertex2 GLfloat)
    vertex $ (Vertex2 1 (-1) :: Vertex2 GLfloat)

escribirPBM :: String -> IO ()
escribirPBM nombre = do
  a <- readPixelArray (0) (0) myWidth2 myHeight2
  writeFile nombre $ "P1\n" ++ show myWidth ++ " " ++ show myHeight ++ "\n" ++ (reverse $ fst $ foldl foldealo ("",1) a)
  where
    foldealo = (\(s,cnt) x -> if cnt==myWidth then (('\n'):((f x):s),1) else ((' '):((f x):s),cnt+1))
    f x = case x .&. 0x00FFFFFF of
      0 -> '1'
      _ -> '0'

display :: IO ()
display = do
  clear [ ColorBuffer ]
  dibujitoCirculo
  dibujarRayitas
  escribirPBM "output.pbm"
  swapBuffers

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialWindowSize $= Size myWidth myHeight
  initialWindowPosition $= Position 400 400
  initialDisplayMode $= [DoubleBuffered,RGBAMode]
  createWindow "probando agn"
  drawBuffer $= FrontAndBackBuffers
  displayCallback $= display
  mainLoop
  
