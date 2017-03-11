-- Retina - Proyecto de Traductores
-- OurMonad, Monad para realizar el analisis de contexto y sus funciones respectivas.
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module OurMonad where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Either
import Data.List

data OurError = OurError Int String | OurErrorNoPos String
instance Error OurError

data OurType = Number | Boolean | Void deriving Eq

instance Show OurType where
  show Number = "number"
  show Boolean = "boolean"
  show Void = "void"


data Scope = Scope {getList::[(String, OurType)]}

data FuncSign = FuncSign {getId::String, getType:: OurType, getParamList::[(String,OurType)]}

data SymTable = SymTable {getScopes::[Scope], getFuncSigns::[FuncSign]}

data OurState = OurState {getSymTable::SymTable, getReturnT::(Maybe OurType)}

emptyState = OurState (SymTable [] []) Nothing

data OurLog = OurLog {getScopesLog::String, getWarningsLog::String}

instance Monoid OurLog where
  mempty = OurLog "" ""
  mappend (OurLog a b) (OurLog c d) = OurLog (a++c) (b++d)

instance Show OurLog where
  show (OurLog a "") = "Alcances:\n"++a++"\n"
  show (OurLog a b) = "Alcances:\n"++a++"\n"++"Advertencias:\n"++b

scopeToOurLog s = OurLog s ""
warningToOurLog s = OurLog "" s


type OurMonad a = StateT OurState (WriterT OurLog (Either OurError)) a

runOurMonad :: OurMonad a -> OurState -> Either OurError ((a, OurState), OurLog)
runOurMonad f a = runWriterT (runStateT f a)

getLog f a = show $ snd $ getRight $ runOurMonad f a `catchError` (\(OurError pos s) -> error $ "\nError en linea "++show pos++":\n"++s)
    where
        getRight (Right x) = x


lookInList :: String -> [(String, OurType)] -> Maybe OurType
lookInList s l = snd <$> find ((==s).fst) l

lookInLastScope :: String -> OurMonad (Maybe OurType)
lookInLastScope s = lookInList s.getList.head'.getScopes.getSymTable <$> get
                    where head' [] = Scope []
                          head' (a:as) = a

lookInSymTable :: String -> OurMonad (Maybe OurType)
lookInSymTable s = msum . map (lookInList s) . (map getList).getScopes.getSymTable <$> get

newScope :: OurMonad ()
newScope = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopes = getScopes oldSymTable
        newSymTable = oldSymTable {getScopes = (Scope []):oldScopes}
    put $ oldState {getSymTable = newSymTable}

removeLastScope :: OurMonad ()
removeLastScope = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        newSymTable = oldSymTable {getScopes = tail.getScopes $ oldSymTable }
    put $ oldState { getSymTable = newSymTable}
        
addToSymTable :: (String, OurType) -> OurMonad ()
addToSymTable pair = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopeList = getScopes oldSymTable
        newLastScope = Scope $ pair:(getList $ head oldScopeList)
        newScopeList = newLastScope:(tail oldScopeList)
        newSymTable = oldSymTable { getScopes = newScopeList }
    put $ oldState { getSymTable = newSymTable }

addFunctionSign :: String -> [(String, OurType)] -> OurType -> OurMonad () -- No crea el scope
addFunctionSign s params typ = do 
    oldState <- get
    let newFunc = FuncSign s typ params
        oldSymTable = getSymTable oldState
        newSymTable = oldSymTable { getFuncSigns = newFunc:(getFuncSigns oldSymTable) }
    put $ oldState { getSymTable = newSymTable }

lookFunction :: String -> OurMonad Bool
lookFunction s = do
    oldState <- get  
    let funcsList = map (getId) (getFuncSigns.getSymTable $ oldState)
        xs = filter (==s) funcsList
        ans = case xs of 
            [] -> False
            _ -> True
    return ans

getFunctionReturnType :: String -> OurMonad OurType
getFunctionReturnType s = do
    oldState <- get
    let listFunc = getFuncSigns.getSymTable $ oldState
        func = filter ((==s).getId) listFunc 
    return $ getType.head $ func

checkFunction :: String -> [OurType] -> OurMonad (Bool,Int)
checkFunction s list = do
    oldState <- get
    let listF = getParamList $ head $ filter ((==s).getId) (getFuncSigns.getSymTable $ oldState)
        listType = map snd listF
    return (listType==list,length listType)

setReturnType :: Maybe OurType -> OurMonad ()
setReturnType typeR = do 
    oldState <- get
    put $ oldState { getReturnT = typeR }

getReturnType :: OurMonad (Maybe OurType)
getReturnType = get >>= (return.getReturnT)

lastScopeToLog :: String -> OurMonad ()
lastScopeToLog scopeName = do
    scopes <- getScopes.getSymTable <$> get
    let nested = length scopes
        lastScope = head scopes
        ident = concat (replicate nested "|   ")
    tell.scopeToOurLog $ ident++"Alcance "++scopeName++":\n"
    tell.scopeToOurLog $ concatMap (\s -> ident++"|> "++s++"\n" ) $ map showVarAndType $ reverse.getList $ lastScope
    where
        showVarAndType (s, t) = s ++" : "++ show t
warningToLog :: String -> OurMonad ()
warningToLog warning = do
    tell.warningToOurLog $ warning++"\n"