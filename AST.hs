-- Retina - Proyecto de Traductores
-- Lexer
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module AST where
import Control.Monad

data Exp =
    PN Exp Exp                          |
    LDFN {listLDFN :: [Exp]}            |
    DFN String Exp Exp                  |
    RDFN String Exp Exp Exp             |
    LPN {listLPN :: [(Exp, String)]}    |
    LPNVN {listLPNVN :: [(Exp, String)]}|
    BooleanN                            |
    NumberN                             |
    TrueN                               |
    FalseN                              |
    ParN Exp                            |
    ComparN Exp String Exp              |
    NotN Exp                            |
    IdN String                          |
    LogicN Exp String Exp               |
    FuncN String Exp                    |
    ExprN Exp                           |
    
    LVN {listLVN :: [Exp]}              |
    LVNVN {listLVNVN :: [Exp]}          |
    
    MinusN Exp                          |
    AritN Exp String Exp                |
    NumberLiteralN String               |
    
    
    LDN {listLDN :: [(Exp, [Exp])]}     |
    DSTN {listDSTN :: [Exp]}            |
    Decl String                         |
    DeclVal String Exp                  |
    
    
    LIN {listLIN :: [Exp]}              |
    
    
    WithDoN Exp Exp                     |
    RepeatN Exp Exp                     |
    AssignN String Exp                  |
    ForN String Exp Exp Exp             |
    ForByN String Exp Exp Exp Exp       |
    IfThenN Exp Exp                     |
    IfThenElseN Exp Exp Exp             |
    WhileN Exp Exp                      |
    WriteN {listWriteN :: [Exp]}        |
    WritelnN {listWritelnN :: [Exp]}    |
    ReadN String                        |
    
    ReturnN Exp                         |
    
    LPWN {listLPWN :: [Exp]}            |
    PWEN Exp                            |
    PWSN String 
    
    
    deriving Show

ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printId n s = putStrLnWithIdent n $ "Identificador: " ++ s

printExp :: Int -> Exp -> IO()
printExp n (PN ldf lblock) = do
    putStrLnWithIdent n "Constructor de Programa:"
    printExp (n+1) ldf
    printExp (n+1) lblock
    
printExp n (LDFN dfl) = do
    putStrLnWithIdent n "Lista de definiciones de funciones:"
    mapM_ (printExp (n+1)) dfl

printExp n (DFN id lp lir) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printExp (n+1) lp
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printExp (n+2) lir
    
    
printExp n (RDFN id lp ret lir) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printExp (n+1) lp
    putStrLnWithIdent (n+1) "Tipo de retorno:"
    printExp (n+2) ret
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printExp (n+2) lir
    
printExp n (LPN pl) = do 
    putStrLnWithIdent n "Lista de parametros de la funcion: "
    mapM_ (printParam) pl
    where 
        printParam (exp, s) = do
            printExp (n+1) exp
            printId (n+1) s
    
printExp n (BooleanN) = do
    putStrLnWithIdent n "Tipo de dato: boolean"
     
printExp n (NumberN) = do
    putStrLnWithIdent n "Tipo de dato: number"
     

printExp n (TrueN) = do
    putStrLnWithIdent n "Literal booleano: true"
     
printExp n (FalseN) = do
    putStrLnWithIdent n "Literal booleano: false"
     
printExp n (ParN exp) = do
    putStrLnWithIdent n "Expresion entre parentesis:"
    printExp (n+1) exp
     
printExp n (ComparN exp s exp1) = do
    putStrLnWithIdent n "Operacion de comparacion:"
    putStrLnWithIdent (n+1) $ "Comparador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (NotN exp) = do
    putStrLnWithIdent n "Negacion booleana:"
    printExp (n+2) exp
    
printExp n (IdN s) = do
    printId n s

printExp n (LogicN exp s exp1) = do
    putStrLnWithIdent n "Operacion binaria logica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (FuncN s exp) = do 
    putStrLnWithIdent n "Llamada de funcion:"
    printId (n+1) s
    printExp (n+1) exp

printExp n (LVN lv) = do
    putStrLnWithIdent n "Lista de valores:"
    mapM_ (printExp (n+1)) lv

printExp n (MinusN exp) = do
    putStrLnWithIdent n "Menos unario:"
    printExp (n+1) exp
    
printExp n (AritN exp s exp1) = do
    putStrLnWithIdent n "Operacion binaria aritmetica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (NumberLiteralN s) = do
    putStrLnWithIdent n $ "Literal numerico: " ++ s
    
printExp n (LDN ld) = do
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
    
printExp n (LIN li) = do 
    putStrLnWithIdent n "Lista de instrucciones:"
    mapM_ (printExp (n+1)) li
    
printExp n (WithDoN exp exp1) = do
    putStrLnWithIdent n "Bloque with-do:"
    printExp (n+1) exp
    printExp (n+1) exp1
    
printExp n (RepeatN exp exp1) = do
    putStrLnWithIdent n "Instruccion repeat:"
    putStrLnWithIdent (n+1) "Cantidad de repeticiones:"
    printExp (n+2) exp
    printExp (n+1) exp1
    
printExp n (AssignN s exp1) = do
    putStrLnWithIdent n "Instruccion de asignacion:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExp (n+2) exp1
    
printExp n (ForN s exp exp1 exp2) = do
    putStrLnWithIdent n "Instruccion for:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExp (n+2) exp1
    printExp (n+1) exp2

printExp n (ForByN s exp exp1 exp2 exp3) = do
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

printExp n (IfThenN exp exp1) = do
    putStrLnWithIdent n "Instruccion if-then:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    printExp (n+1) exp1
    
printExp n (IfThenElseN exp exp1 exp2) = do
    putStrLnWithIdent n "Instruccion if-then-else:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Bloque 1 (then):"
    printExp (n+2) exp1
    putStrLnWithIdent (n+1) "Bloque 2 (else):"
    printExp (n+2) exp2
    
printExp n (WhileN exp exp1) = do
    putStrLnWithIdent n "Instruccion while-do:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    printExp (n+1) exp1

printExp n (WriteN l) = do
    putStrLnWithIdent n "Instruccion de salida:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printExp (n+2)) l
    
printExp n (WritelnN l) = do
    putStrLnWithIdent n "Instruccion de salida con salto:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printExp (n+2)) l

printExp n (ReadN id) = do
    putStrLnWithIdent n "Instruccion de entrada:"
    printId (n+1) id

printExp n (ReturnN exp) = do
    putStrLnWithIdent n "Instruccion de return:"
    printExp (n+1) exp
    
printExp n (LPWN l) = do
    putStrLnWithIdent n "Expresiones:"
    mapM_ (printExp (n+1)) l
    
printExp n (PWSN s) = do
    putStrLnWithIdent n $ "Cadena de caracteres: " ++ s
    
printExp n (PWEN exp) = do
    printExp n exp
    

--NUEEEEEEEEVO 

printExp n (ExprN exp) = do
    putStrLnWithIdent n "Instruccion de expresion:"
    printExp (n+1) exp

