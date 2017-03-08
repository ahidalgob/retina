import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Either

data OurError = Errr
instance Error OurError

data OurType = Number | Boolean deriving Eq

data Scope = Scope {getList::[(String, OurType)]}

data FuncSign = FuncSign {gay::Int}

data SymTable = SymTable {getScopes::[Scope], getFuncS::[FuncSign]}

data OurState = OurState {getSymTable::SymTable, getNestedD::Int, getReturnT::(Maybe OurType)}

type OurMonad a = StateT OurState (WriterT String (Either OurError)) a