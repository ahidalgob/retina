import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Data.Either

data OurError = Errr
instance Error OurError

data OurType = Number | Boolean deriving Eq

data Scope = Scope {getList::[(String, OurType)]}

data FuncSign = FuncSign {getId::String, getType::(Maybe OurType), getParamList::[(String,OurType)]}

data SymTable = SymTable {getScopes::[Scope], getFuncSigns::[FuncSign]}

data OurState = OurState {getSymTable::SymTable, getNestedD::Int, getReturnT::(Maybe OurType)}

type OurMonad a = StateT OurState (WriterT String (Either OurError)) a



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
                         in ((),OurState (SymTable newScopes (getFuncSigns $ getSymTable os)) (getNestedD os) (getReturnT os)))

removeScope :: OurMonad ()
removeScope = state (\os -> let scopes' = tail.getScopes.getSymTable $ os
                            in ((), OurState (SymTable scopes' (getFuncSigns $ getSymTable os)) (getNestedD os) (getReturnT os)))

addToSymTable :: (String, OurType) -> OurMonad ()
addToSymTable pair = state (\os -> let scopes' = getScopes.getSymTable $ os
                                       newScope = Scope $ pair:(getList $ head scopes')
                                       newScopeList = newScope:(tail scopes')
                                   in ((), OurState (SymTable newScopeList (getFuncSigns $ getSymTable os)) (getNestedD os) (getReturnT os)))


addFunctionSign :: String -> [(String, OurType)] -> (Maybe OurType) -> OurMonad () -- No crea el scope
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


setReturnT :: Maybe OurType -> OurMonad ()
setReturnT typeR = state (\os -> ((),OurState (getSymTable os) (getNestedD os) typeR) )
