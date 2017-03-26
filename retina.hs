-- Retina - Proyecto de Traductores
-- Programa Principal
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

import Lexer
import AST
import qualified Parser as P
import ContextChecker
import OurContextMonad
import System.Environment

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
