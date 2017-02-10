module AST where
import Control.Monad

data Exp =
    PE Exp Exp                          |
    LBLOCKE {listLBLOCKE :: [Exp]}      |
    LDFE {listLDFE :: [Exp]}            |
    DFE String Exp Exp                  |
    RDFE String Exp Exp Exp             |
    LPE {listLPE :: [(Exp, String)]}    |
    LPNVE {listLPNVE :: [(Exp, String)]}|
    BooleanE                            |
    NumberE                             |
    ExpA Exp                            |
    ExpB Exp                            |
    TrueE                               |
    FalseE                              |
    ParE Exp                            |
    ComparE Exp String Exp              |
    NotE Exp                            |
    IdE String                          |
    LogicE Exp String Exp               |
    FuncE String Exp                    |
    
    LVE {listLVE :: [Exp]}              |
    LVNVE {listLVNVE :: [Exp]}          |
    
    MinusE Exp                          |
    AritE Exp String Exp                |
    {-NumParE Exp                         |-}
    NumberLiteralE String               |
    
    
    LDE {listLDE :: [(Exp, [Exp])]}     |
    DSTE {listDSTE :: [Exp]}            |
    Decl String                         |
    DeclVal String Exp                  |
    
    
    LIE {listLIE :: [Exp]}              |
    LIRE {listLIRE :: [Exp]}            |
    
    
    WithDoE Exp Exp                     |
    RepeatE Exp Exp                     |
    AssignE String Exp                  |
    ForE String Exp Exp Exp             |
    ForByE String Exp Exp Exp Exp       |
    IfThenE Exp Exp                     |
    IfThenElseE Exp Exp Exp             |
    WhileE Exp Exp                      |
    WriteE {listWriteE :: [Exp]}        |
    ReadE String                        |
    
    
    WithDoRE Exp Exp                    |
    RepeatRE Exp Exp                    |
    ForRE String Exp Exp Exp            |
    ForByRE String Exp Exp Exp Exp      |
    IfThenRE Exp Exp                    |
    IfThenElseRE Exp Exp Exp            |
    WhileRE Exp Exp                     |
    
    ReturnE Exp                         |
    
    LPWE {listLPWE :: [Exp]}            |
    PWEE Exp                            |
    PWSE String 
    
    
	deriving Show

ident = " "

printExp :: Int -> Exp -> IO()
printExp n (PE ldf lblock) = do
    replicateM_ n $ putStr ident
    putStrLn "Constructor de Programa:"
    printExp (n+1) ldf
    -- printExp lblock (n+1)
    
printExp n (LDFE dfl) = do
    replicateM_ n $ putStr ident
    putStrLn "Lista de definiciones de funciones:"
    mapM_ (printExp (n+1)) dfl

printExp n (DFE id lp lir) = do
    replicateM_ n $ putStr ident
    putStrLn "Definicion de funcion:"
    replicateM_ (n+1) $ putStr ident
    putStrLn $ "Identificador: " ++ id
    
    
printExp n (RDFE id lp lir ret) = do
    replicateM_ n $ putStr ident
    putStrLn "Definicion de funcion:"
    replicateM_ (n+1) $ putStr ident
    putStrLn $ "Identificador: " ++ id
    
