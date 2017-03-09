module OurMonad where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Data.Monoid
import Data.Either

data OurError = OurError Int String | OurErrorNoPos String
instance Error OurError

data OurType = Number | Boolean | Void deriving (Eq, Show)

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

getLog f a = show $ snd $ getRight $ runOurMonad f a `catchError` (\(OurError pos s) -> error $ "Error en linea: "++show pos++", "++s)
    where
        getRight (Right x) = x


getNestedDegree :: OurMonad Int
getNestedDegree = state (\os -> (getNestedD os,os))

lookInList :: String -> [(String, OurType)] -> Maybe OurType
lookInList s [] = Nothing
lookInList s (sc:scs) = if (s==(fst sc)) 
                            then Just (snd sc)
                            else lookInList s scs 

lookInLastScope :: String -> OurMonad (Maybe OurType)
lookInLastScope s = state (\os -> (lookInList s $ getList.head'.getScopes.getSymTable $ os,os))
                    where head' [] = Scope []
                          head' (a:as) = a

lookInSymTable :: String -> OurMonad (Maybe OurType)
lookInSymTable s = state (\os -> (msum $ map (lookInList s) (map (getList) (getScopes $ getSymTable os)),os))

newScope :: OurMonad ()
newScope = state (\os -> let newScopes = (Scope []):(getScopes.getSymTable $ os)
                         in ((),OurState (SymTable newScopes (getFuncSigns $ getSymTable os)) ((getNestedD os)+1) (getReturnT os)))

removeScope :: OurMonad ()
removeScope = state (\os -> let scopes' = tail.getScopes.getSymTable $ os
                            in ((), OurState (SymTable scopes' (getFuncSigns $ getSymTable os)) ((getNestedD os)-1) (getReturnT os)))

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


checkFunction :: String -> [OurType] -> OurMonad Bool
checkFunction s list = state (\os -> let listF = getParamList $ head $ filter ((==s).getId) (getFuncSigns.getSymTable $ os)
                                         listType = map snd listF
                                     in (listType==list,os))

getTypeReturn :: String -> OurMonad OurType
getTypeReturn s = state (\os -> let listFunc = getFuncSigns.getSymTable $ os
                                    func = filter ((==s).getId) listFunc 
                                in (getType.head $ func,os))


setReturnT :: Maybe OurType -> OurMonad ()
setReturnT typeR = state (\os -> ((),OurState (getSymTable os) (getNestedD os) typeR) )

lastScopeToLog :: String -> OurMonad ()
lastScopeToLog scopeName = do
    nested <- getNestedDegree 
    OurState (SymTable (sc:_) _) _ _ <- get
    tell.scopeToOurLog $ (replicate (4*nested) ' ')++"Alcance "++scopeName++"\n"
    tell.scopeToOurLog $ concatMap (\s -> (replicate (4*nested+2) ' ')++s++"\n" ) $ map show $ reverse.getList $ sc

warningToLog :: String -> OurMonad ()
warningToLog warning = do
    tell.warningToOurLog $ warning++"\n"