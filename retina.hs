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

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Window

import Data.Word
import Foreign
import Graphics.Rendering.OpenGL

import Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage

import Data.Bits

import System.IO

display :: [Segment] -> Double -> Double -> Double -> Double -> IO ()
display points ww hh dx dy = do
    clear [ ColorBuffer ]
    let pointsToReal = map (\((x,y),(x1,y1))-> ((realToFrac $ (x-dx)/ww,realToFrac $ (y-dy)/hh),(realToFrac $ (x1-dx)/ww,realToFrac $ (y1-dy)/hh))) points
    renderPrimitive Lines $ do
        mapM_ createVertex (pointsToReal :: [((GLfloat, GLfloat),(GLfloat, GLfloat))])
    -- escribir archivo
    swapBuffers
    where createVertex ((x,y),(x1,y1)) = do vertex $ Vertex2 x y
                                            vertex $ Vertex2 x1 y1


main = do
    hSetBuffering stdout NoBuffering
    s <- getArgs >>= (readFile . head)
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
                                displayCallback $= display segments (xx/2) (yy/2) dx dy
                                mainLoop
                                return ()
                            Just e -> error e
        Left e -> putStrLn e
