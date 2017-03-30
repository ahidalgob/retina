-- Retina - Proyecto de Traductores
-- Programa Principal
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

import Lexer
import AST
import RunMonad
import qualified Parser as P
import ContextChecker
import OurContextMonad
import System.Environment
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

display f a = do
    clear [ ColorBuffer ]
    points <- getLogPoints f a
    let pointsToReal = map (\((x,y),(x1,y1))-> ((realToFrac x,realToFrac y),(realToFrac x1,realToFrac y1))) points
    renderPrimitive Lines $ do
        mapM_ createVertex (pointsToReal :: [((GLfloat, GLfloat),(GLfloat, GLfloat))])
    flush
    where createVertex ((x,y),(x1,y1)) = do vertex $ Vertex2 x y
                                            vertex $ Vertex2 x1 y1


main = do
    s <- getArgs >>= (readFile . head)
    let res = runAlexScan s
    case res of
        Right ls -> if invalidTokens ls then do
                        putStrLn "Error lexicografico (alex isn't happy)"
                        mapM_ printToken $ reverse $ tokenList ls
                    else do
                        --printConstrN 0 . (P.parse) . reverse . tokenList $ ls 
                        putStrLn $ getLog (checkConstrN $ (P.parse) . reverse . tokenList $ ls) emptyContextState
        Left e -> putStrLn e
