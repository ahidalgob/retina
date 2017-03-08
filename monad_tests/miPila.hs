import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Either

data MyError =  Error1 String | Error2
                deriving Show

instance Error MyError


-- runWriter (runErrorT (runStateT xx1 0))
-- devuelve un 
-- (Either MyError ((), Int), String)
xx1 :: StateT Int (ErrorT MyError (Writer String)) ()
xx1 = put 5


-- runStateT xx2 0
-- devuelve un
-- Either MyError ((), Int)

xx2 :: StateT Int (Either MyError) ()
xx2 = put 5


-- runWriterT (runStateT xx3 0)
-- devuelve un
-- Either MyError (((), Int), String)          SI CUADRA
xx3 :: StateT Int (WriterT String (Either MyError)) ()
xx3 = put 5



type MyMonad a = StateT Int (WriterT String (Either MyError)) a

runMyMonad :: MyMonad a -> Int -> Either MyError ((a, Int), String)
runMyMonad f a = runWriterT (runStateT f a)

getLog f a = snd $ getRight $ runMyMonad f a `catchError` (\e -> return $ (((), 0), show e ++ "\n"))
    where
        getRight (Right x) = x


main :: IO ()
main = do
    putStrLn "getLog f1 0"
    putStrLn $ getLog f1 0


    putStrLn "getLog f2 0"
    putStrLn $ getLog f2 0

    putStrLn "getLog (f3 0) 0"
    putStrLn $ getLog (f3 0) 0

    putStrLn "getLog (f3 0) 2"
    putStrLn $ getLog (f3 0) 2

    putStrLn "getLog (loopTo 5) 2"
    putStrLn $ getLog (loopTo 5) 2

    putStrLn "getLog (loopTo 0) 2"
    putStrLn $ getLog (loopTo 0) 2
    return ()


f1 :: MyMonad ()
f1 = do
    tell "hola\n"
    x <- get
    tell $ show x ++ "\n"
    modify (+3)
    x <- get
    tell $ show x ++ "\n"
    return ()


f2 :: MyMonad ()
f2 = do
    tell "hola\n"
    x <- get
    tell $ show x ++ "\n"
    modify (+3)
    x <- get
    tell $ show x ++ "\n"
    throwError $ Error1 "se prendio"
    return ()

f3 :: Int -> MyMonad ()
f3 x = do
    y <- get
    when (x == y) $ throwError Error2
    tell "Todo bien\n"
    return ()


loopTo :: Int -> MyMonad ()
loopTo to = do
    x <- get

    if (x > to)
        then throwError $ Error1 "Sin Sentido"
        else do 
            tell $ show x ++ "\n"
            if(x==to)
                then return ()
                else do
                    modify (+1)
                    loopTo to