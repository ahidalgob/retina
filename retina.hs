import Lexer
import qualified Parser as P
import System.Environment

main :: IO ()

main = do
    s <- getArgs >>= (readFile . head)
    let res = runAlexScan s
    case res of
        Right ls -> if invalidTokens ls then do
                        putStrLn "Error lexicografico (alex isn't happy)"
                        mapM_ printToken $ reverse $ tokenList ls
                    else do
                        putStrLn ":)"
                        putStrLn . show . (P.parse) . reverse . tokenList $ ls
        Left e -> putStrLn e
