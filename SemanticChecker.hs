module SemanticChecker where
import AST
import OurMonad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Either

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
                        when (te /= t) $ throwError $ OurErrorNoPos ("Tipo de la variable \""++s++"\" y tipo de su valor no coinciden en su declaracion.")


checkInstrListN :: InstrListN -> OurMonad Bool
checkInstrListN (LIN instrList) = do
    --resList <- mapM checkInstrN instrList
    --let res = all id resList
    --return res
    (all id) <$> (mapM checkInstrN instrList)

checkInstrN :: InstrN -> OurMonad Bool
checkInstrN _ = do
    return True

checkExpN :: ExpN -> OurMonad OurType

checkExpN (TrueN) = do
    return Boolean

checkExpN (FalseN) = do
    return Boolean

checkExpN (ParN exp) = do
    ans <- checkExpN exp
    return ans
