-- Retina - Proyecto de Traductores
-- Lexer
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

import Lexer
import AST
import qualified Parser as P
import SemanticChecker
import OurMonad
import System.Environment

main = do
    s <- getArgs >>= (readFile . head)
    let res = runAlexScan s
    case res of
        Right ls -> if invalidTokens ls then do
                        putStrLn "Error lexicografico (alex isn't happy)"
                        mapM_ printToken $ reverse $ tokenList ls
                    else do
                        --Imprimir AST
                        --printConstrN 0 . (P.parse) . reverse . tokenList $ ls 
                        putStrLn $ getLog (checkConstrN $ (P.parse) . reverse . tokenList $ ls) emptyState
                        
        Left e -> putStrLn e
