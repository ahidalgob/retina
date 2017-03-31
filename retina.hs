-- Retina - Proyecto de Traductores
-- Programa Principal
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

import Lexer
import AST
import RunAST
import RunMonad
import qualified Parser as P
import ContextChecker
import OurContextMonad
import System.Environment

import Control.Monad

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Window

import Data.Word
import Foreign
import Graphics.Rendering.OpenGL

import Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage

import Data.Bits

import System.IO


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

writePBM :: String -> Int -> Int -> IO ()
writePBM fileName ww hh = do
  a <- readPixelArray (0) (0) ww hh
  writeFile fileName $ "P1\n" ++ show ww ++ " " ++ show hh ++ "\n" ++ (concat.map (reverse.(\s -> '\n':s))) (fst $ foldl foldealo ([""],1) a)
  where
    foldealo (s:ls,cnt) x = if cnt==ww then ("":(((f x):s):ls),1) else (((' '):((f x):s)):ls,cnt+1)
    f x = case x .&. 0x00FFFFFF of
      0 -> '1'
      _ -> '0'

display :: [Segment] -> Double -> Double -> Double -> Double -> String -> IO ()
display points ww hh dx dy programName = do
    clear [ ColorBuffer ]
    let pointsToReal = map (\((x,y),(x1,y1))-> ((realToFrac $ (x-dx)/ww,realToFrac $ (y-dy)/hh),(realToFrac $ (x1-dx)/ww,realToFrac $ (y1-dy)/hh))) points
    renderPrimitive Lines $ do
        mapM_ createVertex (pointsToReal :: [((GLfloat, GLfloat),(GLfloat, GLfloat))])
    writePBM ("retina-"++programName++".pbm") (floor $ ww*2) (floor $ hh*2) 
    swapBuffers
    where createVertex ((x,y),(x1,y1)) = do vertex $ Vertex2 x y
                                            vertex $ Vertex2 x1 y1


main = do
    hSetBuffering stdout NoBuffering
    programFile <- getArgs >>= (return.head)
    let programName = extractName programFile 
    when (length programName < 5) $ error "El archivo debe ser un archivo .rtn"
    let ext = reverse.take 4.reverse $ programName
    when (ext/=".rtn") $ error "El archivo debe ser un archivo .rtn"
    s <- readFile programFile
    let res = runAlexScan s
    case res of
        Right ls -> if invalidTokens ls then do
                        putStrLn "Error lexicografico (alex isn't happy)"
                        mapM_ printToken $ reverse $ tokenList ls
                    else do
                        --printConstrN 0 . (P.parse) . reverse . tokenList $ ls
                        let ast = (P.parse) . reverse . tokenList $ ls
                        case getContextError (checkConstrN ast) emptyContextState of
                            Nothing -> do
                                result <- runRunMonad (runConstrN ast) ourEmptyState
                                let segments = ((0,0),(0,0)):snd result
                                    state = snd $ fst result
                                    x1 = getMaxRight state - getMaxLeft state + 20
                                    y1 = getMaxUp state - getMaxDown state + 20
                                    xx = max 200 $ min x1 1002
                                    yy = max 200 $ min y1 1002
                                    dx = (getMaxRight state + getMaxLeft state)/2
                                    dy = (getMaxUp state + getMaxDown state)/2
                                _ <- getArgsAndInitialize
                                initialWindowSize $= Size (floor xx) (floor yy)
                                initialWindowPosition $= Position 400 400
                                initialDisplayMode $= [DoubleBuffered,RGBAMode]
                                createWindow "retina AGN"
                                drawBuffer $= FrontAndBackBuffers
                                displayCallback $= display segments (xx/2) (yy/2) dx dy ((reverse.drop 4.reverse)programName)
                                mainLoop
                                return ()
                            Just e -> error e
        Left e -> putStrLn e
    where
        extractName :: String -> String
        extractName s = reverse $ takeWhile ((/=) '/') $ reverse s