module SemanticChecker where
import AST
import OurMonad
import Data.Traversable
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Data.Either

typeNConvert :: TypeN -> OurType
typeNConvert BooleanN = Boolean
typeNConvert NumberN = Number

checkConstrN :: ConstrN -> OurMonad ()

checkConstrN (PN ldfn lin) = do
    checkConstrN ldfn
    --checkInstrListN lin

checkConstrN (LDFN []) = do
    return ()

checkConstrN (LDFN (funcDefN:rest)) = do
    let (funId, paramList, instrListN, lineNum, maybeRet) = case funcDefN of
            DFN s p i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, Nothing)
            RDFN s p ret i (ln,c) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln, Just $ typeNConvert ret)
    
    repeated <- lookFunction funId

    when (repeated) $ throwError $ Errr lineNum $ "Funcion "++funId++" redefinida."

    addFunctionSign funId paramList maybeRet
    setReturnT maybeRet

    
    newScope
    mapM_ (adder funId lineNum) paramList
    lastScopeToLog $ '_':funId
    
    -- checkInstrListN instrListN

    removeScope
    setReturnT Nothing
    checkConstrN $ LDFN rest
    where adder funId lineNum (s, t) = do   x <- lookInLastScope s
                                            case x of
                                                Nothing -> addToSymTable (s, t)
                                                (Just _) -> throwError $ Errr lineNum ("Variable "++s++" definida dos veces en parametros de funcion "++funId++".")




-- el encargado de abrir y cerrar el scope (with) debe ser el que haga
-- newScope y removeScope
checkConstrN (LDN []) = do
    return ()

checkConstrN (LDN ((tN,varNList):rest)) = do
    let t = typeNConvert tN
    -- agregar variables al scope
    -- lanzar error con pos cualquiera y en el with catcharlo y lanzarlo bien
    checkConstrN $ LDN rest