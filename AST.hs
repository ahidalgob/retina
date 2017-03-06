-- Retina - Proyecto de Traductores
-- Lexer
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module AST where
import Control.Monad

data Exp =
    PE Exp Exp                          |
    LDFE {listLDFE :: [Exp]}            |
    DFE String Exp Exp                  |
    RDFE String Exp Exp Exp             |
    LPE {listLPE :: [(Exp, String)]}    |
    LPNVE {listLPNVE :: [(Exp, String)]}|
    BooleanE                            |
    NumberE                             |
    TrueE                               |
    FalseE                              |
    ParE Exp                            |
    ComparE Exp String Exp              |
    NotE Exp                            |
    IdE String                          |
    LogicE Exp String Exp               |
    FuncE String Exp                    |
    ExprE Exp                           |
    
    LVE {listLVE :: [Exp]}              |
    LVNVE {listLVNVE :: [Exp]}          |
    
    MinusE Exp                          |
    AritE Exp String Exp                |
    NumberLiteralE String               |
    
    
    LDE {listLDE :: [(Exp, [Exp])]}     |
    DSTE {listDSTE :: [Exp]}            |
    Decl String                         |
    DeclVal String Exp                  |
    
    
    LIE {listLIE :: [Exp]}              |
    
    
    WithDoE Exp Exp                     |
    RepeatE Exp Exp                     |
    AssignE String Exp                  |
    ForE String Exp Exp Exp             |
    ForByE String Exp Exp Exp Exp       |
    IfThenE Exp Exp                     |
    IfThenElseE Exp Exp Exp             |
    WhileE Exp Exp                      |
    WriteE {listWriteE :: [Exp]}        |
    WritelnE {listWritelnE :: [Exp]}    |
    ReadE String                        |
    
    ReturnE Exp                         |
    
    LPWE {listLPWE :: [Exp]}            |
    PWEE Exp                            |
    PWSE String 
    
    
    deriving Show

ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printId n s = putStrLnWithIdent n $ "Identificador: " ++ s

printExp :: Int -> Exp -> IO()
printExp n (PE ldf lblock) = do
    putStrLnWithIdent n "Constructor de Programa:"
    printExp (n+1) ldf
    printExp (n+1) lblock
    
printExp n (LDFE dfl) = do
    putStrLnWithIdent n "Lista de definiciones de funciones:"
    mapM_ (printExp (n+1)) dfl

printExp n (DFE id lp lir) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printExp (n+1) lp
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printExp (n+2) lir
    
    
printExp n (RDFE id lp ret lir) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printExp (n+1) lp
    putStrLnWithIdent (n+1) "Tipo de retorno:"
    printExp (n+2) ret
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printExp (n+2) lir
    
printExp n (LPE pl) = do 
    putStrLnWithIdent n "Lista de parametros de la funcion: "
    mapM_ (printParam) pl
    where 
        printParam (exp, s) = do
            printExp (n+1) exp
            printId (n+1) s
    
printExp n (BooleanE) = do
    putStrLnWithIdent n "Tipo de dato: boolean"
     
printExp n (NumberE) = do
    putStrLnWithIdent n "Tipo de dato: number"
     

printExp n (TrueE) = do
    putStrLnWithIdent n "Literal booleano: true"
     
printExp n (FalseE) = do
    putStrLnWithIdent n "Literal booleano: false"
     
printExp n (ParE exp) = do
    putStrLnWithIdent n "Expresion entre parentesis:"
    printExp (n+1) exp
     
printExp n (ComparE exp s exp1) = do
    putStrLnWithIdent n "Operacion de comparacion:"
    putStrLnWithIdent (n+1) $ "Comparador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (NotE exp) = do
    putStrLnWithIdent n "Negacion booleana:"
    printExp (n+2) exp
    
printExp n (IdE s) = do
    printId n s

printExp n (LogicE exp s exp1) = do
    putStrLnWithIdent n "Operacion binaria logica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (FuncE s exp) = do 
    putStrLnWithIdent n "Llamada de funcion:"
    printId (n+1) s
    printExp (n+1) exp

printExp n (LVE lv) = do
    putStrLnWithIdent n "Lista de valores:"
    mapM_ (printExp (n+1)) lv

printExp n (MinusE exp) = do
    putStrLnWithIdent n "Menos unario:"
    printExp (n+1) exp
    
printExp n (AritE exp s exp1) = do
    putStrLnWithIdent n "Operacion binaria aritmetica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (NumberLiteralE s) = do
    putStrLnWithIdent n $ "Literal numerico: " ++ s
    
printExp n (LDE ld) = do
    putStrLnWithIdent n "Declaraciones de datos:"
    mapM_ (printDatos) ld
    where 
        printDatos (exp, ldst) = do
            printExp (n+1) exp
            mapM_ (printExp (n+2)) ldst
            


printExp n (Decl s) = do
    putStrLnWithIdent n "Declaracion con identificador:"
    printId (n+1) s

printExp n (DeclVal s exp) = do
    putStrLnWithIdent n "Declaracion con identificador y valor:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExp (n+2) exp
    
printExp n (LIE li) = do 
    putStrLnWithIdent n "Lista de instrucciones:"
    mapM_ (printExp (n+1)) li
    
printExp n (WithDoE exp exp1) = do
    putStrLnWithIdent n "Bloque with-do:"
    printExp (n+1) exp
    printExp (n+1) exp1
    
printExp n (RepeatE exp exp1) = do
    putStrLnWithIdent n "Instruccion repeat:"
    putStrLnWithIdent (n+1) "Cantidad de repeticiones:"
    printExp (n+2) exp
    printExp (n+1) exp1
    
printExp n (AssignE s exp1) = do
    putStrLnWithIdent n "Instruccion de asignacion:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExp (n+2) exp1
    
printExp n (ForE s exp exp1 exp2) = do
    putStrLnWithIdent n "Instruccion for:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExp (n+2) exp1
    printExp (n+1) exp2

printExp n (ForByE s exp exp1 exp2 exp3) = do
    putStrLnWithIdent n "Instruccion for-by:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExp (n+2) exp1
    putStrLnWithIdent (n+1) "Paso:"
    printExp (n+2) exp2
    printExp (n+1) exp3

printExp n (IfThenE exp exp1) = do
    putStrLnWithIdent n "Instruccion if-then:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    printExp (n+1) exp1
    
printExp n (IfThenElseE exp exp1 exp2) = do
    putStrLnWithIdent n "Instruccion if-then-else:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Bloque 1 (then):"
    printExp (n+2) exp1
    putStrLnWithIdent (n+1) "Bloque 2 (else):"
    printExp (n+2) exp2
    
printExp n (WhileE exp exp1) = do
    putStrLnWithIdent n "Instruccion while-do:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    printExp (n+1) exp1

printExp n (WriteE l) = do
    putStrLnWithIdent n "Instruccion de salida:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printExp (n+2)) l
    
printExp n (WritelnE l) = do
    putStrLnWithIdent n "Instruccion de salida con salto:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printExp (n+2)) l

printExp n (ReadE id) = do
    putStrLnWithIdent n "Instruccion de entrada:"
    printId (n+1) id

printExp n (ReturnE exp) = do
    putStrLnWithIdent n "Instruccion de return:"
    printExp (n+1) exp
    
printExp n (LPWE l) = do
    putStrLnWithIdent n "Expresiones:"
    mapM_ (printExp (n+1)) l
    
printExp n (PWSE s) = do
    putStrLnWithIdent n $ "Cadena de caracteres: " ++ s
    
printExp n (PWEE exp) = do
    printExp n exp
    

--NUEEEEEEEEVO 

printExp n (ExprE exp) = do
    putStrLnWithIdent n "Instruccion de expresion:"
    printExp (n+1) exp

