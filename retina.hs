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

display :: [Segment] -> Double -> Double -> IO ()
display points ww hh = do
    clear [ ColorBuffer ]
    let pointsToReal = map (\((x,y),(x1,y1))-> ((realToFrac $ x/ww,realToFrac $ y/hh),(realToFrac $ x1/ww,realToFrac $ y1/hh))) points
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
                                let segments = snd result
                                    state = snd $ fst result
                                    xx = max (getMaxRight state) (-(getMaxLeft state)) + 10
                                    yy = max (getMaxUp state) (-(getMaxDown state)) + 10
                                --print segments -- DEBUG
                                --print myWidth --DEBUG
                                --print myHeight --DEBUG

                                _ <- getArgsAndInitialize
                                initialWindowSize $= Size (floor xx) (floor yy)
                                initialWindowPosition $= Position 400 400
                                initialDisplayMode $= [DoubleBuffered,RGBAMode]
                                createWindow "retina AGN"
                                drawBuffer $= FrontAndBackBuffers
                                displayCallback $= display segments xx yy
                                mainLoop
                                return ()
                            Just e -> error e
        Left e -> putStrLn e
