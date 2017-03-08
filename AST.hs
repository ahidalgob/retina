-- Retina - Proyecto de Traductores
-- Lexer
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module AST where
import Control.Monad

data Exp =
    IdN String                          |
    TrueN                               |
    FalseN                              |
    ParN Exp                            |
    ComparN Exp String Exp (Int,Int)            |
    NotN Exp (Int,Int)                         |
    LogicN Exp String Exp (Int,Int)              |
    FuncN String ExpList                |
    MinusN Exp                          |
    AritN Exp String Exp (Int,Int)              |
    NumberLiteralN String
    deriving Show

data Type =
    BooleanN                            |
    NumberN
    deriving Show

data Constr = 
    PN Constr InstrList                 |
    LDFN {listLDFN :: [FuncDef]}        |
    LDN {listLDN :: [(Type, [Var])]}
    deriving Show

data FuncDef =
    DFN String ParamList InstrList (Int,Int)    |
    RDFN String ParamList Type InstrList (Int,Int)
    deriving Show

data ParamList = 
    LPN {listLPN :: [(Type, String)]}    |
    LPNVN {listLPNVN :: [(Type, String)]}
    deriving Show

data ExpList =
    LEN {listLEN :: [Exp]}              |
    LENVN {listLENVN :: [Exp]}
    deriving Show

data VarList =
    LVarN {listLVN :: [Var]}
    deriving Show

data Var =
    VarN String                          |
    VarValN String Exp
    deriving Show

data InstrList = 
    LIN {listLIN :: [Instr]}
    deriving Show

data WordList =
    LPWN {listLPWN :: [Word]}
    deriving Show

data Word =
    PWEN Exp                            |
    PWSN String
    deriving Show


data Instr =
    WithDoN Constr InstrList (Int,Int)                   |
    RepeatN Exp InstrList                     |
    AssignN String Exp                  |
    ForN String Exp Exp InstrList             |
    ForByN String Exp Exp Exp InstrList       |
    IfThenN Exp InstrList (Int,Int)                    |
    IfThenElseN Exp InstrList InstrList (Int,Int)            |
    WhileN Exp InstrList                      |
    WriteN {listWriteN :: [Word]}        |
    WritelnN {listWritelnN :: [Word]}    |
    ReadN String                        |
    ReturnN Exp (Int,Int)                     |
    ExprN Exp
    deriving Show

ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printId n s = putStrLnWithIdent n $ "Identificador: " ++ s

printExp :: Int -> Exp -> IO()
printExp n (IdN s) = do
    printId n s

printExp n (TrueN) = do
    putStrLnWithIdent n "Literal booleano: true"
     
printExp n (FalseN) = do
    putStrLnWithIdent n "Literal booleano: false"

printExp n (ParN exp) = do
    putStrLnWithIdent n "Expresion entre parentesis:"
    printExp (n+1) exp
     
printExp n (ComparN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion de comparacion:"
    putStrLnWithIdent (n+1) $ "Comparador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (NotN exp _) = do
    putStrLnWithIdent n "Negacion booleana:"
    printExp (n+2) exp

printExp n (LogicN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion binaria logica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (FuncN s exp) = do 
    putStrLnWithIdent n "Llamada de funcion:"
    printId (n+1) s
    printExpList (n+1) exp

printExp n (MinusN exp) = do
    putStrLnWithIdent n "Menos unario:"
    printExp (n+1) exp
    
printExp n (AritN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion binaria aritmetica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printExp n (NumberLiteralN s) = do
    putStrLnWithIdent n $ "Literal numerico: " ++ s


printType :: Int -> Type -> IO()
printType n (BooleanN) = do
    putStrLnWithIdent n "Tipo de dato: boolean"
     
printType n (NumberN) = do
    putStrLnWithIdent n "Tipo de dato: number"


printConstr :: Int -> Constr -> IO()
printConstr n (PN ldf lblock) = do
    putStrLnWithIdent n "Constructor de Programa:"
    printConstr (n+1) ldf
    printInstrList (n+1) lblock
    
printConstr n (LDFN dfl) = do
    putStrLnWithIdent n "Lista de definiciones de funciones:"
    mapM_ (printFuncDef (n+1)) dfl

printConstr n (LDN ld) = do
    putStrLnWithIdent n "Declaraciones de datos:"
    mapM_ printDatos ld
    where 
        printDatos (exp, ldst) = do
            printType (n+1) exp
            mapM_ (printVar (n+2)) ldst


printFuncDef :: Int -> FuncDef -> IO()
printFuncDef n (DFN id lp lir _) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printParamList (n+1) lp
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printInstrList (n+2) lir    
    
printFuncDef n (RDFN id lp ret lir _) = do
    putStrLnWithIdent n "Definicion de funcion:"
    printId (n+1) id
    printParamList (n+1) lp
    putStrLnWithIdent (n+1) "Tipo de retorno:"
    printType (n+2) ret
    putStrLnWithIdent (n+1) "Cuerpo de la funcion:"
    printInstrList (n+2) lir
    

printParamList :: Int -> ParamList -> IO()
printParamList n (LPN pl) = do 
    putStrLnWithIdent n "Lista de parametros de la funcion: "
    mapM_ (printParam) pl
    where 
        printParam (exp, s) = do
            printType (n+1) exp
            printId (n+1) s
    

printExpList :: Int -> ExpList -> IO()
printExpList n (LEN lv) = do
    putStrLnWithIdent n "Lista de expresiones:"
    mapM_ (printExp (n+1)) lv


printVar :: Int -> Var -> IO()
printVar n (VarN s) = do
    putStrLnWithIdent n "Declaracion con identificador:"
    printId (n+1) s

printVar n (VarValN s exp) = do
    putStrLnWithIdent n "Declaracion con identificador y valor:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExp (n+2) exp
    
printInstrList :: Int -> InstrList -> IO()
printInstrList n (LIN li) = do 
    putStrLnWithIdent n "Lista de instrucciones:"
    mapM_ (printInstr (n+1)) li
    
printInstr :: Int -> Instr -> IO()
printInstr n (WithDoN exp exp1 _) = do
    putStrLnWithIdent n "Bloque with-do:"
    printConstr (n+1) exp
    printInstrList (n+1) exp1
    
printInstr n (RepeatN exp exp1) = do
    putStrLnWithIdent n "Instruccion repeat:"
    putStrLnWithIdent (n+1) "Cantidad de repeticiones:"
    printExp (n+2) exp
    printInstrList (n+1) exp1
    
printInstr n (AssignN s exp1) = do
    putStrLnWithIdent n "Instruccion de asignacion:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor:"
    printExp (n+2) exp1
    
printInstr n (ForN s exp exp1 exp2) = do
    putStrLnWithIdent n "Instruccion for:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExp (n+2) exp1
    printInstrList (n+1) exp2

printInstr n (ForByN s exp exp1 exp2 exp3) = do
    putStrLnWithIdent n "Instruccion for-by:"
    putStrLnWithIdent (n+1) "Variable iteradora:"
    printId (n+2) s
    putStrLnWithIdent (n+1) "Desde:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Hasta:"
    printExp (n+2) exp1
    putStrLnWithIdent (n+1) "Paso:"
    printExp (n+2) exp2
    printInstrList (n+1) exp3

printInstr n (IfThenN exp exp1 _) = do
    putStrLnWithIdent n "Instruccion if-then:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    printInstrList (n+1) exp1
    
printInstr n (IfThenElseN exp exp1 exp2 _) = do
    putStrLnWithIdent n "Instruccion if-then-else:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Bloque 1 (then):"
    printInstrList (n+2) exp1
    putStrLnWithIdent (n+1) "Bloque 2 (else):"
    printInstrList (n+2) exp2
    
printInstr n (WhileN exp exp1) = do
    putStrLnWithIdent n "Instruccion while-do:"
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    printInstrList (n+1) exp1

printInstr n (WriteN l) = do
    putStrLnWithIdent n "Instruccion de salida:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printWord (n+2)) l
    
printInstr n (WritelnN l) = do
    putStrLnWithIdent n "Instruccion de salida con salto:"
    putStrLnWithIdent (n+1) "Lista de expresiones:"
    mapM_ (printWord (n+2)) l

printInstr n (ReadN id) = do
    putStrLnWithIdent n "Instruccion de entrada:"
    printId (n+1) id

printInstr n (ReturnN exp _) = do
    putStrLnWithIdent n "Instruccion de return:"
    printExp (n+1) exp

printInstr n (ExprN exp) = do
    putStrLnWithIdent n "Instruccion de expresion:"
    printExp (n+1) exp


printWordList :: Int -> WordList -> IO()
printWordList n (LPWN l) = do
    putStrLnWithIdent n "Expresiones:"
    mapM_ (printWord (n+1)) l
    

printWord :: Int -> Word -> IO()
printWord n (PWSN s) = do
    putStrLnWithIdent n $ "Cadena de caracteres: " ++ s
    
printWord n (PWEN exp) = do
    printExp n exp
    

