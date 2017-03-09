module SemanticChecker where
import AST
import OurMonad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Either
import Data.Maybe

data Returned = Yes | Idk | No deriving Eq

(|+|) :: Returned -> Returned -> Returned
(|*|) :: Returned -> Returned -> Returned
Yes |+| _ = Yes
_ |+| Yes = Yes
Idk |+| _ = Idk
_ |+| Idk = Idk
No |+| No = No

No |*| _ = No
_ |*| No = No
Idk |*| _ = Idk
_ |*| Idk = Idk
Yes |*| Yes = Yes



typeNConvert :: TypeN -> OurType
typeNConvert BooleanN = Boolean
typeNConvert NumberN = Number

checkConstrN :: ConstrN -> OurMonad ()
checkConstrN (PN ldfN instrListN) = do
    checkConstrN ldfN
    checkInstrListN instrListN
    return ()

checkConstrN (LDFN []) = do
    return ()

checkConstrN (LDFN (funcDefN:rest)) = do
    let (funId, paramList, instrListN, lineNum, retType) = case funcDefN of
            DFN s p i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, Void)
            RDFN s p ret i (ln,c) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, typeNConvert ret)
    
    repeated <- lookFunction funId

    when (repeated) $ throwError $ OurError lineNum $ "Funcion "++funId++" redefinida."

    addFunctionSign funId paramList retType
    setReturnType $ Just retType

    newScope
    mapM_ (adder funId lineNum) paramList
    lastScopeToLog $ '_':funId

    returned <- checkInstrListN instrListN
    when (retType /= Void) $ case returned of
        Yes -> return ()
        Idk -> warningToLog $ "linea "++show lineNum++": Funcion "++funId++" puede no alcanzar un return."
        No -> throwError $ OurError lineNum $ "Funcion "++funId++" no tiene instruccion de return."
    removeScope
    setReturnType Nothing
    checkConstrN $ LDFN rest
    where adder funId lineNum (s, t) = do   x <- lookInLastScope s
                                            case x of
                                                Nothing -> addToSymTable (s, t)
                                                (Just _) -> throwError $ OurError lineNum ("Variable \""++s++"\" definida dos veces en parametros de funcion "++funId++".")


-- el encargado de abrir y cerrar el scope (with) debe ser el que haga
-- newScope y removeScope
checkConstrN (LDN []) = do
    return ()

checkConstrN (LDN ((tN,varNList):rest)) = do
    let t = typeNConvert tN
    mapM_ (adder t) varNList -- lanza error sin pos y en el with catcharlo y lanzarlo bien
    checkConstrN $ LDN rest
    where
        adder t varN = do 
            let (s,me) = case varN of
                    (VarN st) -> (st, Nothing)
                    (VarValN st e) -> (st, Just e)

            case me of
                    (Just e) -> do
                        te <- checkExpN e
                        when (te /= t) $ throwError $ OurErrorNoPos ("Tipo de la variable \""++s++"\" y tipo de su valor no coinciden en su declaracion.")
                    Nothing -> return ()
            x <- lookInLastScope s
            case x of
                    Nothing -> addToSymTable (s, t)
                    (Just _) -> throwError $ OurErrorNoPos ("Variable \""++s++"\" definida dos veces en bloque with-do.")
            

checkInstrListN :: InstrListN -> OurMonad Returned
checkInstrListN (LIN instrList) = do
    (foldl (|+|) No) <$> (mapM checkInstrN instrList)

checkInstrN :: InstrN -> OurMonad Returned
checkInstrN (WithDoN ldn lin (lineNum,_)) = do
    newScope
    checkConstrN ldn `catchError` (reThrow lineNum)
    lastScopeToLog "bloque with-do"
    res <- checkInstrListN lin
    removeScope
    return res
    where
        reThrow :: Int -> OurError -> OurMonad ()
        reThrow lineNum (OurErrorNoPos s) = throwError $ OurError lineNum s
        reThrow lineNum e = throwError e

checkInstrN (RepeatN expn lin (lineNum,_)) = do
    et <- checkExpN expn
    when (et /= Number) $ throwError $ OurError lineNum $ "La expresion de una instruccion repeat x times debe ser de tipo number. (Tipo encontrado: "++show et++")" 
    checkInstrListN lin

checkInstrN (AssignN s expn (lineNum,_)) = do
    found <- lookInSymTable s
    when (found == Nothing) $ throwError $ OurError lineNum $ "'"++s++"' no esta declarada en este alcance."
    et <- checkExpN expn 
    when (et /= fromJust found) $ throwError $ OurError lineNum ("Se esperaba expresion de tipo "++show (fromJust found)++" en lado derecho de asignacion (Tipo encontrado: "++show et++")")
    return No

checkInstrN (ForN s expn1 expn2 lin (lineNum,_)) = do
    e1t <- checkExpN expn1
    e2t <- checkExpN expn2
    when (e1t /= Number || e2t /= Number) $ throwError $ OurError lineNum $"Las expresiones de un For deben ser de tipo number (Tipos encontrados ("++show e1t++","++show e2t++"))"
    newScope
    addToSymTable (s, Number)
    lastScopeToLog "for"
    res <- checkInstrListN lin
    removeScope
    return res

checkInstrN (ForByN s expn1 expn2 expn3 lin (lineNum,_)) = do
    e1t <- checkExpN expn1
    e2t <- checkExpN expn2
    e3t <- checkExpN expn3
    when (e1t /= Number || e2t /= Number || e3t /= Number) $ throwError $ OurError lineNum $ "Las expresiones de un For con Salto deben ser de tipo number (Tipos encontrados ("++show e1t++","++show e2t++","++show e3t++"))"
    newScope
    addToSymTable (s, Number)
    lastScopeToLog "for-by"
    res <- checkInstrListN lin
    removeScope
    return res

checkInstrN (IfThenN expn lin (lineNum,_)) = do
    et <- checkExpN expn
    when (et /= Boolean) $ throwError $ OurError lineNum $ "Se esperaba tipo boolean en condicion de if (tipo encontrado: "++show et++")"
    --res <- checkInstrListN lin
    --return $ Idk |*| res
    ((|*|) Idk) <$> checkInstrListN lin

checkInstrN (IfThenElseN expn lin1 lin2 (lineNum,_)) = do
    et <- checkExpN expn
    when (et /= Boolean) $ throwError $ OurError lineNum $ "Se esperaba tipo boolean en condicion de if (tipo encontrado: "++show et++")"
    ret1 <- checkInstrListN lin1
    ret2 <- checkInstrListN lin2
    case (ret1, ret2) of
        (Yes, Yes) -> return Yes
        (No, No) -> return No
        (_, _) -> return Idk

checkInstrN (WhileN expn lin (lineNum,_)) = do
    et <- checkExpN expn
    when (et /= Boolean) $ throwError $ OurError lineNum $ "Se esperaba tipo boolean en condicion de while (tipo encontrado: "++show et++")"
    ((|*|) Idk) <$> checkInstrListN lin

checkInstrN (WriteN wordList (lineNum,_)) = do
    checkWordListN wordList `catchError` (\(OurErrorNoPos s) -> throwError $ OurError lineNum $ s)
    return No

checkInstrN (WritelnN wordList (lineNum,_)) = do
    checkWordListN wordList `catchError` (\(OurErrorNoPos s) -> throwError $ OurError lineNum $ s)
    return No 

checkInstrN (ReadN s (lineNum, _)) = do
    found <- lookInSymTable s
    when (found == Nothing) $ throwError $ OurError lineNum $ "Variable del read fuera de alcance"
    return No

checkInstrN (ReturnN expN (lineNum, _)) = do
    rt <- getReturnType 
    when (rt == Nothing) $ throwError $ OurError lineNum $ "No puede haber instrucciones de return fuera de una funcion."
    when (rt == Just Void) $ throwError $ OurError lineNum $ "No puede haber instrucciones de return en una funcion de tipo void."
    et <- checkExpN expN
    when (fromJust rt /= et) $ throwError $ OurError lineNum $ "Return esperaba tipo "++show rt++"pero la expresion tiene tipo"++show et++"."
    return Yes

checkInstrN (ExprN expN) = do
    et <- checkExpN expN
    when (et/=Void) $ warningToLog $ "Expresion con valor sin efecto en linea ?? ."
    return No



checkExpN :: ExpN -> OurMonad OurType

checkExpN (IdN s (lineNum,_)) = do
    bo <- lookInSymTable s
    when (bo==Nothing) $ throwError $ OurError lineNum $ "Variable \""++s++"\" no definida."
    return (fromJust bo)

checkExpN (TrueN _) = do
    return Boolean

checkExpN (FalseN _) = do
    return Boolean

checkExpN (ParN exp (lineNum,_)) = do
    t <- checkExpN exp
    when (t==Void) $ throwError $ OurError lineNum $ ("La expresion entre parentesis no evalua a nada.")
    return t

checkExpN (ComparN exp s exp1 (lineNum,_)) = do
    t <- checkExpN exp
    t1 <- checkExpN exp1
    when (t1==Void && t==Void) $ throwError $ OurError lineNum $ "Los parametros de la comparacion "++s++" evaluan a void." 
    when (t1/=t) $ throwError $ OurError lineNum $ "Tipos de las expresiones de la comparacion "++s++" no concuerdan ( "++show t++","++show t1++")." 
    when ((s/="==" && s/="/=") && (t==Boolean || t1==Boolean))  $ throwError $ OurError lineNum $ "En operador de comparacion "++s++" no acepta parametros de tipo boolean"  
    return Boolean

checkExpN (NotN exp (lineNum,_)) = do
    t <- checkExpN exp
    when (t==Void) $ throwError $ OurError lineNum $ "El operador not espera un boolean y recibe un void." 
    when (t/=Boolean) $ throwError $ OurError lineNum $ "El operador not espera un boolean y recibe un number." 
    return t

checkExpN (LogicN exp s exp1 (lineNum,_)) = do
    t <- checkExpN exp
    t1 <- checkExpN exp1
    when (t/=Boolean || t1/=Boolean) $ throwError $ OurError lineNum $ "El operador "++s++" espera un (boolean, boolean) y recibio ("++show t++","++show t++")."  
    return t

checkExpN (FuncN s expList (lineNum,_)) = do
    bo <-lookFunction s
    when (not bo) $ throwError $ OurError lineNum $ "Funcion "++s++" no definida."
    newList <- mapM checkExpN (listLEN expList)
    let bo1 = any (==Void) newList
    when (bo1) $ throwError $ OurError lineNum $ "En la funcion "++s++" hay parametros que no retornan nada."
    bo3 <- checkFunction s newList 
    when (not bo3) $ throwError $ OurError lineNum $ "Tipos de los parametros de la funcion "++s++" no concuerdan." 
    getTypeReturn s

checkExpN (MinusN exp (lineNum,_)) = do
    t <- checkExpN exp
    when (t/=Number) $ throwError $ OurError lineNum $ "El operador Minus espera un Number y recibe un "++(show t)++"." 
    return t

checkExpN (AritN exp s exp1 (lineNum,_)) = do
    t <- checkExpN exp
    t1 <- checkExpN exp1
    when (t/=Number || t1/=Number) $ throwError $ OurError lineNum $ "El operador "++s++" espera un (number,number) y recibe un ("++show t++","++show t1++")."
    return t

checkExpN (NumberLiteralN s _) = do
    return Number

checkWordListN :: [WordN] -> OurMonad ()

checkWordListN (wordList) = do
    mapM_ fun wordList
    where fun (PWEN exp) = do
            t <- checkExpN exp
            case t of 
                Void -> throwError $ OurErrorNoPos ("La expresion a mostrar en pantalla no evalua a nada.")
                _ -> return ()    
          fun _ = return ()
