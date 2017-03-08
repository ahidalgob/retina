

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
    tell "hola\n"
    checkConstrN ldfn
    --checkInstrListN lin

checkConstrN (LDFN []) = do
    return ()

checkConstrN (LDFN (funcDefN:rest)) = do
    case funcDefN of
        DFN s paramListN _ _ -> do
            addFunctionSign s ((map (\(x,y) -> (y, typeNConvert x))).listLPN $ paramListN) Nothing
            
        RDFN s paramListN typeN _ _ -> do
            addFunctionSign s ((map (\(x,y) -> (y, typeNConvert x))).listLPN $ paramListN) (Just $ typeNConvert typeN)
            setReturnT $ Just $ typeNConvert typeN

    let (funId, paramListN, instrListN, lineNum) = case funcDefN of
            DFN s p i (ln,_) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln)
            RDFN s p _ i (ln,c) -> (s,(map (\(x,y) -> (y, typeNConvert x))).listLPN $ p,i,ln)

    -- checkInstrListN instrListN
    newScope
    mapM_ (adder funId lineNum) paramListN
    lastScopeToLog $ '_':funId
    
    setReturnT Nothing
    checkConstrN $ LDFN rest

    where adder funId lineNum (s, t) = do   x <- lookInLastScope s
                                            case x of
                                                Nothing -> addToSymTable (s, t)
                                                (Just _) -> throwError $ Errr ("Variable "++s++" definida dos veces en funcion "++funId++" en linea "++show lineNum++".")