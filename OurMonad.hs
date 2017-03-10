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

data OurState = OurState {getSymTable::SymTable, getNestedD::Int, getReturnT::(Maybe OurType)}

emptyState = OurState (SymTable [] []) (0) Nothing

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


getNestedDegree :: OurMonad Int
getNestedDegree = getNestedD <$> get

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
        oldNestedD = getNestedD oldState
        newSymTable = oldSymTable {getScopes = (Scope []):oldScopes}
    put $ oldState {getSymTable = newSymTable, getNestedD = oldNestedD + 1}

removeLastScope :: OurMonad ()
removeLastScope = do
        oldState <- get
        let oldSymTable = getSymTable oldState
            oldNestedD = getNestedD oldState
            newSymTable = oldSymTable {getScopes = tail.getScopes $ oldSymTable }
        put $ oldState { getSymTable = newSymTable, getNestedD = oldNestedD -1}
        
addToSymTable :: (String, OurType) -> OurMonad ()
addToSymTable pair = state (\os -> let scopes' = getScopes.getSymTable $ os
                                       newScope = Scope $ pair:(getList $ head scopes')
                                       newScopeList = newScope:(tail scopes')
                                   in ((), OurState (SymTable newScopeList (getFuncSigns $ getSymTable os)) (getNestedD os) (getReturnT os)))

addFunctionSign :: String -> [(String, OurType)] -> OurType -> OurMonad () -- No crea el scope
addFunctionSign s params typ = state (\os -> let newFunc = FuncSign s typ params
                                                 newSymTable = SymTable (getScopes.getSymTable $ os) (newFunc:(getFuncSigns.getSymTable $ os))
                                             in ((),OurState newSymTable (getNestedD os) (getReturnT os))) 

lookFunction :: String -> OurMonad Bool
lookFunction s = state (\os -> let funcsList = map (getId) (getFuncSigns.getSymTable $ os)
                                   xs = filter (==s) funcsList
                                   ans = case xs of 
                                            [] -> False
                                            _ -> True
                               in (ans,os))

getFunctionReturnType :: String -> OurMonad OurType
getFunctionReturnType s = state (\os -> let listFunc = getFuncSigns.getSymTable $ os
                                            func = filter ((==s).getId) listFunc 
                                        in (getType.head $ func,os))


setReturnType :: Maybe OurType -> OurMonad ()
setReturnType typeR = state (\os -> ((),OurState (getSymTable os) (getNestedD os) typeR) )

getReturnType :: OurMonad (Maybe OurType)
getReturnType = get >>= (return.getReturnT)

lastScopeToLog :: String -> OurMonad ()
lastScopeToLog scopeName = do
    nested <- getNestedDegree 
    OurState (SymTable (sc:_) _) _ _ <- get
    let ident = concat (replicate nested "|   ")
    tell.scopeToOurLog $ ident++"Alcance "++scopeName++":\n"
    tell.scopeToOurLog $ concatMap (\s -> ident++"> "++s++"\n" ) $ map showVarAndType $ reverse.getList $ sc
    where showVarAndType (s, t) = s ++" : "++ show t
warningToLog :: String -> OurMonad ()
warningToLog warning = do
    tell.warningToOurLog $ warning++"\n"