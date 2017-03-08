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

data SymTable = SymTable {getScopes::[Scope], getFuncS::[FuncSign]}

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
                         in ((),OurState (SymTable newScopes (getFuncS $ getSymTable os)) (getNestedD os) (getReturnT os)))

removeScope :: OurMonad ()
removeScope = state (\os -> let scopes' = tail.getScopes.getSymTable $ os
                            in ((), OurState (SymTable scopes' (getFuncS $ getSymTable os)) (getNestedD os) (getReturnT os)))

addToSymTable :: (String, OurType) -> OurMonad ()
addToSymTable pair = state (\os -> let scopes' = getScopes.getSymTable $ os
                                       newScope = Scope $ pair:(getList $ head scopes')
                                       newScopeList = newScope:(tail scopes')
                                   in ((), OurState (SymTable newScopeList (getFuncS $ getSymTable os)) (getNestedD os) (getReturnT os)))

