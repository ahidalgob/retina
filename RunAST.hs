
module RunAST where

import RunMonad
import OurType
import AST
import Control.Monad


----------------------------------------------------------
-- runConstrN ------------------------------------------
----------------------------------------------------------
runConstrN :: ConstrN -> RunMonad ()
runConstrN PN ldfn iln = do --Construccion de Programa
    --declarar funciones predefinidas
    runConstrN ldfn
    runInstrListN iln

runConstrN (LDFN l) = do --Construccion de Lista de Definicion de Funciones
    mapM_ runFuncDefN l

runConstrN (LDN l) = do --Construccion de Lista de Declaracion de variables
    mapM_ runTypeAndList l
    where 
        runTypeAndList :: (TypeN, [VarN]) -> RunMonad ()
        runTypeAndList (tN, varNList) = do
            let t = typeNConvert tN
            mapM_ (adder t) varNList
        adder :: OurType -> VarN -> RunMonad ()
        adder t varN = do
            let (s, me) = case varN of
                    (VarN st) -> (st, Nothing)
                    (VarValN st e) -> (st, Just e)
                val = case me of
                    Just e -> runExpN e
                    Nothing -> case t of
                        Boolean -> BooleanVal False
                        Number -> NumberVal 0
            addToSymTable (s,val,False,t)


----------------------------------------------------------
-- runFuncDefN -----------------------------------------
----------------------------------------------------------
runFuncDefN :: FuncDefN -> RunMonad ()
runFuncDefN funcDefN = do
    let (funId, paramList, instrListN, retType) = case funcDefN of
            DFN s p i _ -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i, Void)
            RDFN s p ret i _ -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i, typeNConvert ret)

    addToFundec (funId, map fst paramList, listLIN instrListN, retType)


----------------------------------------------------------
-- runInstrListN ---------------------------------------
----------------------------------------------------------
runInstrListN :: InstrListN -> RunMonad (Maybe Val)
runInstrListN (LIN instrList) = do
    mapM runInstrN instrList -- NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO RETURNSSSSSS


----------------------------------------------------------
-- runInstrN -------------------------------------------
----------------------------------------------------------
repeatCycle n lin = do
    if n > 0
        then do ret <- runInstrListN lin
                case ret of
                    Nothing -> repeatCycle (n-1) lin
                    Just x -> return $ Just x
        else return Nothing

forCycle s it to by lin = do
    if it <= to
        then do
            ret <- runInstrListN lin
            case ret of
                Nothing -> do
                    changeValInSymTable (s, NumberVal (it+1))
                    forCycle s (it+1) to by lin
                Just x -> return $ Just x
        else return Nothing

whileCycle expn lin = do
    BooleanVal b <- runExpN expn
    if b
        then do 
            ret <- runInstrListN lin
            case ret of
                Nothing -> whileCycle expn lin
                Just x -> return $ Just x
        else return Nothing

runInstrN :: InstrN -> RunMonad (Maybe Val)
runInstrN (WithDoN ldn lin _) = do
    newScope
    runConstrN ldn
    ret <- runInstrListN lin
    removeLastScope
    return ret 

runInstrN (RepeatN expn lin _) = do
    NumberVal n <- runExpN expn
    repeatCycle n lin
    

runInstrN (AssignN s expn _) = do
    Just (_,_,mutable,_) <- lookInSymTable s
    when (not mutable) $ error ":( no mutable"
    val <- runExpN expn
    changeValInSymTable (s, val)
    return Nothing

runInstrN (ForN s expn1 expn2 lin p) = do
    runInstrN $ ForByN s expn1 expn2 (NumberLiteralN "1") lin p

runInstrN (ForByN s expn1 expn2 expn3 lin _) = do
    NumberVal e1 <- runExpN expn1
    NumberVal e2 <- runExpN expn2
    NumberVal e3 <- runExpN expn3
    newScope
    addToSymTable (s,NumberVal e1,False,Number)
    ret <- forCycle s e1 e2 e3 lin
    removeLastScope
    return ret

runInstrN (IfThenN expn lin _) = do
    BooleanVal b <- runExpN expn
    if (b == True) 
        then runInstrListN lin
        else return Nothing

runInstrN (IfThenElseN expn lin1 lin2 _) = do
    BooleanVal b <- runExpN expn
    if (b == True) 
        then runInstrListN lin1
        else runInstrListN lin2

runInstrN (WhileN expn lin _) = do
    whileCycle expn lin

checkInstrN (WriteN wordList _) = return Nothing
 ---------- FALTAAAAAAAAA

checkInstrN (WritelnN wordList _) = return Nothing
 ---------- FALTAAAAAAAAA

checkInstrN (ReadN s _) = return Nothing
 ---------- FALTAAAAAAAAA

checkInstrN (ReturnN expn _) = return $ Just $ runExpN expn

checkInstrN (ExprN expn) = do
    runExpN expn
    return Nothing