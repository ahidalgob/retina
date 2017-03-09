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

data Returned = Yes | Idk | No

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
    setReturnT $ Just retType

    
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
                        when (te /= t) $ throwError $ OurErrorNoPos ("Tipo de la variable \""++s++"\" y tipo de su valor no coinciden en su declaracion.")
                    Nothing -> return ()

checkInstrListN :: InstrListN -> OurMonad Returned
checkInstrListN (LIN instrList) = do
    (foldl (|+|) No) <$> (mapM checkInstrN instrList)

checkInstrN :: InstrN -> OurMonad Returned
checkInstrN (WithDoN ldn lin (lineNum,_)) = do
    newScope
    checkConstrN ldn `catchError` (\(OurErrorNoPos s) -> throwError $ OurError lineNum s)
    lastScopeToLog "bloque with-do"
    removeScope
    return Yes

checkInstrN _ = return Yes

checkExpN :: ExpN -> OurMonad OurType

checkExpN (IdN s (lineNum,_)) = do
    bo <- lookInSymTable s
    when (bo==Nothing) $ throwError $ OurError lineNum $ "Variable "++s++" no definida."
    return (fromJust bo)

checkExpN (TrueN) = do
    return Boolean

checkExpN (FalseN) = do
    return Boolean

checkExpN (ParN exp) = do
    ans <- checkExpN exp
    when (ans==Void) $ throwError $ OurErrorNoPos ("La expresion entre parentesis no evalua a nada.")
    return ans

checkExpN (ComparN exp s exp1 (lineNum,_)) = do
    ans <- checkExpN exp
    ans1 <- checkExpN exp1
    when (ans1==Void && ans==Void) $ throwError $ OurError lineNum $ "Los parametros de la comparacion "++s++" evaluan a void." 
    when (ans1/=ans) $ throwError $ OurError lineNum $ "Tipos de las expresiones de la comparacion "++s++" no concuerdan ( "++show ans++" "++show ans1++")." 
    return ans

checkExpN (NotN exp (lineNum,_)) = do
    ans <- checkExpN exp
    when (ans==Void) $ throwError $ OurError lineNum $ "El operador not espera un boolean y recibe un void." 
    when (ans/=Boolean) $ throwError $ OurError lineNum $ "El operador not espera un boolean y recibe un number." 
    return ans

checkExpN (LogicN exp s exp1 (lineNum,_)) = do
    ans <- checkExpN exp
    ans1 <- checkExpN exp1
    when (ans/=Boolean || ans1/=Boolean) $ throwError $ OurError lineNum $ "El operador "++s++"espera un (boolean, boolean) y recibio ("++show ans++","++show ans++")."  
    return ans

checkExpN (FuncN s expList (lineNum,_)) = do
    bo <-lookFunction s
    when (not bo) $ throwError $ OurError lineNum $ "Funcion "++s++" no definida."
    newList <- mapM checkExpN (listLEN expList)
    let bo1 = any (==Void) newList
    when (bo1) $ throwError $ OurError lineNum $ "No puedes tener parametros que no retornen nada en la funcion."
    bo3 <- checkFunction s newList 
    when (not bo3) $ throwError $ OurError lineNum $ "Tipos de los parametros de la funcion "++s++" no concuerdan." 
    getTypeReturn s

checkExpN (MinusN exp (lineNum,_)) = do
    ans <- checkExpN exp
    when (ans/=Number) $ throwError $ OurError lineNum $ "El operador Minus espera un Number y recibe un "++(show ans)++"." 
    return ans

checkExpN (AritN exp s exp1 (lineNum,_)) = do
    ans <- checkExpN exp
    ans1 <- checkExpN exp1
    when (ans/=Number || ans1/=Number) $ throwError $ OurError lineNum $ "El operador "++s++" espera un (number,number) y recibe un ("++show ans++","++show ans1++")."
    return ans

checkExpN (NumberLiteralN s) = do
    return Number

