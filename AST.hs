-- Retina - Proyecto de Traductores
-- Lexer
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module AST where
import Control.Monad

data ExpN =
    IdN String                                      |
    TrueN                                           |
    FalseN                                          |
    ParN ExpN                                       |
    ComparN ExpN String ExpN (Int,Int)              |
    NotN ExpN (Int,Int)                             |
    LogicN ExpN String ExpN (Int,Int)               |
    FuncN String ExpListN                           |
    MinusN ExpN                                     |
    AritN ExpN String ExpN (Int,Int)                |
    NumberLiteralN String
    deriving Show

data TypeN =
    BooleanN                                        |
    NumberN
    deriving Show

data ConstrN = 
    PN ConstrN InstrListN                           |
    LDFN {listLDFN :: [FuncDefN]}                   |
    LDN {listLDN :: [(TypeN, [VarN])]}
    deriving Show

data FuncDefN =
    DFN String ParamListN InstrListN (Int,Int)      |
    RDFN String ParamListN TypeN InstrListN (Int,Int)
    deriving Show

data ParamListN = 
    LPN {listLPN :: [(TypeN, String)]}              |
    LPNVN {listLPNVN :: [(TypeN, String)]}
    deriving Show

data ExpListN =
    LEN {listLEN :: [ExpN]}                         |
    LENVN {listLENVN :: [ExpN]}
    deriving Show

data VarListN =
    LVarN {listLVN :: [VarN]}
    deriving Show

data VarN =
    VarN String                                     |
    VarValN String ExpN
    deriving Show

data InstrListN = 
    LIN {listLIN :: [InstrN]}
    deriving Show

data WordListN =
    LPWN {listLPWN :: [WordN]}
    deriving Show

data WordN =
    PWEN ExpN                                       |
    PWSN String
    deriving Show


data InstrN =
    WithDoN ConstrN InstrListN (Int,Int)                        |
    RepeatN ExpN InstrListN                                     |
    AssignN String ExpN                                         |
    ForN String ExpN ExpN InstrListN                            |
    ForByN String ExpN ExpN ExpN InstrListN                     |
    IfThenN ExpN InstrListN (Int,Int)                           |
    IfThenElseN ExpN InstrListN InstrListN (Int,Int)            |
    WhileN ExpN InstrListN                                      |
    WriteN {listWriteN :: [WordN]}                              |
    WritelnN {listWritelnN :: [WordN]}                          |
    ReadN String                                                |
    ReturnN ExpN (Int,Int)                                      |
    ExprN ExpN
    deriving Show

ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printId n s = putStrLnWithIdent n $ "Identificador: " ++ s

printExpN :: Int -> ExpN -> IO()
printExpN n (IdN s) = do
    printId n s

printExpN n (TrueN) = do
    putStrLnWithIdent n "Literal booleano: true"
     
printExpN n (FalseN) = do
    putStrLnWithIdent n "Literal booleano: false"

printExpN n (ParN exp) = do
    putStrLnWithIdent n "Expresion entre parentesis:"
    printExpN (n+1) exp
     
printExpN n (ComparN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion de comparacion:"
    putStrLnWithIdent (n+1) $ "Comparador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1
    
printExpN n (NotN exp _) = do
    putStrLnWithIdent n "Negacion booleana:"
    printExpN (n+2) exp

printExpN n (LogicN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion binaria logica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1
    
printExpN n (FuncN s exp) = do 
    putStrLnWithIdent n "Llamada de funcion:"
    printId (n+1) s
    printExpListN (n+1) exp

printExpN n (MinusN exp) = do
    putStrLnWithIdent n "Menos unario:"
    printExpN (n+1) exp
    
printExpN n (AritN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion binaria aritmetica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1
    
printExpN n (NumberLiteralN s) = do
    putStrLnWithIdent n $ "Literal numerico: " ++ s


printTypeN :: Int -> TypeN -> IO()
printTypeN n (BooleanN) = do
    putStrLnWithIdent n "Tipo de dato: boolean"
     
printTypeN n (NumberN) = do
    putStrLnWithIdent n "Tipo de dato: number"


printConstrN :: Int -> ConstrN -> IO()
printConstrN n (PN ldf lblock) = do
    putStrLnWithIdent n "Constructor de Programa:"
    printConstrN (n+1) ldf
    printInstrListN (n+1) lblock
    
printConstrN n (LDFN dfl) = do
    putStrLnWithIdent n "Lista de definiciones de funciones:"
    mapM_ (printFuncDefN (n+1)) dfl

printConstrN n (LDN ld) = do
    putStrLnWithIdent n "Declaraciones de datos:"
    mapM_ printDatos ld
    where 
        printDatos (exp, ldst) = do
            printTypeN (n+1) exp
            mapM_ (printVarN (n+2)) ldst


printFuncDefN :: Int -> FuncDefN -> IO()
printFuncDefN n (DFN id lp lir _) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printParamListN (n+1) lp
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printInstrListN (n+2) lir    
    
printFuncDefN n (RDFN id lp ret lir _) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printParamListN (n+1) lp
    putStrLnWithIdent (n+1) "Tipo de retorno:"
    printTypeN (n+2) ret
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printInstrListN (n+2) lir
    

printParamListN :: Int -> ParamListN -> IO()
printParamListN n (LPN pl) = do 
    putStrLnWithIdent n "Lista de parametros de la funcion: "
    mapM_ (printParam) pl
    where 
        printParam (exp, s) = do
            printTypeN (n+1) exp
            printId (n+1) s
    

printExpListN :: Int -> ExpListN -> IO()
printExpListN n (LEN lv) = do
    putStrLnWithIdent n "Lista de expresiones:"
    mapM_ (printExpN (n+1)) lv


printVarN :: Int -> VarN -> IO()
printVarN n (VarN s) = do
    putStrLnWithIdent n "Declaracion con identificador:"
    printId (n+1) s

printVarN n (VarValN s exp) = do
    putStrLnWithIdent n "Declaracion con identificador y valor:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExpN (n+2) exp
    
printInstrListN :: Int -> InstrListN -> IO()
printInstrListN n (LIN li) = do 
    putStrLnWithIdent n "Lista de instrucciones:"
    mapM_ (printInstrN (n+1)) li
    
printInstrN :: Int -> InstrN -> IO()
printInstrN n (WithDoN exp exp1 _) = do
    putStrLnWithIdent n "Bloque with-do:"
    printConstrN (n+1) exp
    printInstrListN (n+1) exp1
    
printInstrN n (RepeatN exp exp1) = do
    putStrLnWithIdent n "Instruccion repeat:"
    putStrLnWithIdent (n+1) "Cantidad de repeticiones:"
    printExpN (n+2) exp
    printInstrListN (n+1) exp1
    
printInstrN n (AssignN s exp1) = do
    putStrLnWithIdent n "Instruccion de asignacion:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExpN (n+2) exp1
    
printInstrN n (ForN s exp exp1 exp2) = do
    putStrLnWithIdent n "Instruccion for:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExpN (n+2) exp1
    printInstrListN (n+1) exp2

printInstrN n (ForByN s exp exp1 exp2 exp3) = do
    putStrLnWithIdent n "Instruccion for-by:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExpN (n+2) exp1
    putStrLnWithIdent (n+1) "Paso:"
    printExpN (n+2) exp2
    printInstrListN (n+1) exp3

printInstrN n (IfThenN exp exp1 _) = do
    putStrLnWithIdent n "Instruccion if-then:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExpN (n+2) exp
    printInstrListN (n+1) exp1
    
printInstrN n (IfThenElseN exp exp1 exp2 _) = do
    putStrLnWithIdent n "Instruccion if-then-else:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Bloque 1 (then):"
    printInstrListN (n+2) exp1
    putStrLnWithIdent (n+1) "Bloque 2 (else):"
    printInstrListN (n+2) exp2
    
printInstrN n (WhileN exp exp1) = do
    putStrLnWithIdent n "Instruccion while-do:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExpN (n+2) exp
    printInstrListN (n+1) exp1

printInstrN n (WriteN l) = do
    putStrLnWithIdent n "Instruccion de salida:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printWordN (n+2)) l
    
printInstrN n (WritelnN l) = do
    putStrLnWithIdent n "Instruccion de salida con salto:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printWordN (n+2)) l

printInstrN n (ReadN id) = do
    putStrLnWithIdent n "Instruccion de entrada:"
    printId (n+1) id

printInstrN n (ReturnN exp _) = do
    putStrLnWithIdent n "Instruccion de return:"
    printExpN (n+1) exp

printInstrN n (ExprN exp) = do
    putStrLnWithIdent n "Instruccion de expresion:"
    printExpN (n+1) exp


printWordListN :: Int -> WordListN -> IO()
printWordListN n (LPWN l) = do
    putStrLnWithIdent n "Expresiones:"
    mapM_ (printWordN (n+1)) l
    

printWordN :: Int -> WordN -> IO()
printWordN n (PWSN s) = do
    putStrLnWithIdent n $ "Cadena de caracteres: " ++ s
    
printWordN n (PWEN exp) = do
    printExpN n exp
    

