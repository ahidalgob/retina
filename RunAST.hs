
module RunAST where

import RunMonad
import OurType
import AST


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
    let (funId, paramList, instrListN, lineNum, retType) = case funcDefN of
            DFN s p i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, Void)
            RDFN s p ret i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, typeNConvert ret)

    addToFundec (funId, map fst paramList, listLIN instrListN, retType)


----------------------------------------------------------
-- runInstrListN ---------------------------------------
----------------------------------------------------------
runInstrListN :: InstrListN -> RunMonad ()
runInstrListN (LIN instrList) = do
    mapM runInstrN instrList


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
    