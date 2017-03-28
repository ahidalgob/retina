
module RunAST where

import RunMonad
import OurType
import AST


----------------------------------------------------------
-- runConstrN ------------------------------------------
----------------------------------------------------------
runConstrN :: ConstrN -> RunMonad ()
runConstrN (PN ldfn iln) = do --Construccion de Programa
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
            case me of
                Just e -> do
                    val <- runExpN e
                    addToSymTable (s,val,False,t)
                Nothing -> do
                    let val = case t of
                            Boolean -> BooleanVal False
                            Number -> NumberVal 0
                    addToSymTable (s,val,True,t)

----------------------------------------------------------
-- runFuncDefN -----------------------------------------
----------------------------------------------------------
runFuncDefN :: FuncDefN -> RunMonad ()
runFuncDefN funcDefN = do
    let (funId, paramList, instrListN, lineNum, retType) = case funcDefN of
            DFN s p i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, Void)
            RDFN s p ret i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, typeNConvert ret)

    addToFundec (funId, map fst paramList, listLIN instrListN, retType)


----------------------------------------------------------
-- runInstrListN ---------------------------------------
----------------------------------------------------------
runInstrListN :: InstrListN -> RunMonad ()
runInstrListN (LIN instrList) = do
    mapM_ runInstrN instrList


----------------------------------------------------------
-- runInstrN -------------------------------------------
----------------------------------------------------------
repeatCycle n lin = do
    if n > 0
        then runInstrListN lin
        else return ()

runInstrN :: InstrN -> RunMonad ()
runInstrN (WithDoN ldn lin _) = do
    newScope
    runConstrN ldn
    runInstrListN lin
    removeLastScope

runInstrN (RepeatN expn lin _) = do
    NumberVal n <- runExpN expn
    repeatCycle n lin

-- runInstrN (AssignN s expn _) = do
    



----------------------------------------------------------
-- runExpN ------------------------------------------
----------------------------------------------------------

runExpN :: ExpN -> RunMonad Val

runExpN (IdN s _) = do thrd' <$> lookInSymTable s

runExpN (TrueN _) = return (BooleanVal True)

runExpN (FalseN _) = return (BooleanVal False)
    
runExpN (ParN expn _) = runExpN expn

runExpN (ComparN exp0 s exp1 _) = do
    valExp0 <- runExpN exp0
    valExp1 <- runExpN exp1
    case (valExp0,valExp1) of 
        (NumberVal ex0,NumberVal ex1) ->
            case s of
                "<" -> return $ BooleanVal (ex0 < ex1)
                ">" -> return $ BooleanVal (ex0 > ex1)
                "<=" -> return $ BooleanVal (ex0 <= ex1)
                ">=" -> return $ BooleanVal (ex0 >= ex1)
                "==" -> return $ BooleanVal (ex0 == ex1)
                _ -> return $ BooleanVal (ex0 /= ex1)
        (BooleanVal ex0,BooleanVal ex1) ->
            case s of
                "==" -> return $ BooleanVal (ex0 == ex1)
                _ -> return $ BooleanVal (ex0 /= ex1)

runExpN (NotN expn _) = do
    BooleanVal exp0 <- runExpN expn
    return $ BooleanVal (not exp0)


