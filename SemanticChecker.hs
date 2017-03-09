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
    let (funId, paramList, instrListN, lineNum, maybeRet) = case funcDefN of
            DFN s p i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, Nothing)
            RDFN s p ret i (ln,c) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, Just $ typeNConvert ret)
    
    repeated <- lookFunction funId

    when (repeated) $ throwError $ OurError lineNum $ "Funcion "++funId++" redefinida."

    addFunctionSign funId paramList maybeRet
    setReturnT maybeRet

    
    newScope
    mapM_ (adder funId lineNum) paramList
    lastScopeToLog $ '_':funId

    checkInstrListN instrListN

    removeScope
    setReturnT Nothing
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
            x <- lookInLastScope s
            case x of
                    Nothing -> addToSymTable (s, t)
                    (Just _) -> throwError $ OurErrorNoPos ("Variable \""++s++"\" definida dos veces en bloque with-do.")
            case me of
                    (Just e) -> do
                        te <- checkExpN e
                        when (te /= Just t) $ throwError $ OurErrorNoPos ("Tipo de la variable \""++s++"\" y tipo de su valor no coinciden en su declaracion.")



checkInstrListN :: InstrListN -> OurMonad Bool
checkInstrListN (LIN instrList) = do
    --resList <- mapM checkInstrN instrList
    --let res = all id resList
    --return res
    (all id) <$> (mapM checkInstrN instrList)

checkInstrN :: InstrN -> OurMonad Bool
checkInstrN _ = do
    return True

checkExpN :: ExpN -> OurMonad (Maybe OurType)

checkExpN (TrueN) = do
    return $ Just Boolean

checkExpN (FalseN) = do
    return $ Just Boolean

checkExpN (ParN exp) = do
    ans <- checkExpN exp
    return ans

checkExpN (ComparN exp s exp1 (lineNum,_)) = do
    ans <- checkExpN exp
    ans1 <- checkExpN exp1
    when (ans1/=ans) $ throwError $ OurError lineNum $ "Tipos de las expresiones de la comparacion "++s++" no concuerdan ( "++show ans++" "++show ans1++")." 
    return ans

checkExpN (NotN exp (lineNum,_)) = do
    ans <- checkExpN exp
    when (ans/=Just Boolean) $ throwError $ OurError lineNum $ "El operador not espera un Boolean y recibe un Number." 
    return ans

checkExpN (LogicN exp s exp1 (lineNum,_)) = do
    ans <- checkExpN exp
    ans1 <- checkExpN exp1
    when (ans/=Just Boolean) $ throwError $ OurError lineNum $ "El operador "++s++"espera un Boolean y el primer elemento es un Number." 
    when (ans1/=Just Boolean) $ throwError $ OurError lineNum $ "El operador "++s++"espera un Boolean y el segundo elemento es un Number."  
    return ans




checkExpN (FuncN s expList (lineNum,_)) = do
    bo <-lookFunction s
    when (not bo) $ throwError $ OurError lineNum $ "Funcion "++s++" no definida."
    newList <- mapM checkExpN (listLEN expList)
    let bo1 = any (==Nothing) newList
    when (bo1) $ throwError $ OurError lineNum $ "No puedes tener parametros que no retornen nada en la funcion."
    bo2 <- lookFunction s
    when (not bo2) $ throwError $ OurError lineNum $ "Funcion "++s++" no definida."
    let newValList = map (fromJust) newList
    bo3 <- checkFunction s newValList 
    when (not bo3) $ throwError $ OurError lineNum $ "Tipos de los parametros de la funcion "++s++" no concuerdan." 
    ans <- getTypeReturn s
    return ans
