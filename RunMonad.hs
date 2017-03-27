-- Retina - Proyecto de Traductores
-- OurMonad, Monad para realizar el analisis de contexto y sus funciones respectivas.
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module RunMonad where

import AST
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Either
import Data.List

data CursorStatus = On | Off deriving (Show,Eq)

data Val = BooleanVal Bool | NumberVal Double deriving (Show,Eq)

data OurType = Number | Boolean | Void deriving Eq

type Pos = (Double, Double)

type Mutable = Bool

type Direction = Double

type VarDescript = (String,Val,Mutable,OurType)

data Cursor = Cursor {getPos:: Pos, getDirection::Direction, getStatus::CursorStatus }

data Scope = Scope {getList::[VarDescript]}

data SymTable = SymTable {getScopes::[Scope]}

data OurState = OurState {getSymTable::SymTable, getCursor::Cursor}

data FuncDec = FuncDec{ getDec:: [(String,[String],[InstrN],OurType)]}

type RunMonad a = StateT OurState (WriterT [Int] IO ) a

addToSymTable :: VarDescript -> RunMonad ()
addToSymTable pair = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopeList = getScopes oldSymTable
        newLastScope = Scope $ pair:(getList $ head oldScopeList)
        newScopeList = newLastScope:(tail oldScopeList)
        newSymTable = oldSymTable { getScopes = newScopeList }
    put $ oldState { getSymTable = newSymTable }

fst' :: VarDescript -> String
fst' (s,_,_,_) = s

lookInList :: String -> [VarDescript] -> Maybe VarDescript
lookInList s l = find ((==s).fst') l

lookInSymTable :: String -> RunMonad (Maybe VarDescript)
lookInSymTable s = msum . map (lookInList s) . (map getList).getScopes.getSymTable <$> get

removeLastScope :: RunMonad ()
removeLastScope = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        newSymTable = oldSymTable {getScopes = tail.getScopes $ oldSymTable }
    put $ oldState { getSymTable = newSymTable}

newScope :: RunMonad ()
newScope = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopes = getScopes oldSymTable
        newSymTable = oldSymTable {getScopes = (Scope []):oldScopes}
    put $ oldState {getSymTable = newSymTable}

 --CAMBIAR VALOR SYMTABLE FALTA LEER 

getSymTable' :: RunMonad (SymTable)
getSymTable' = do
    getSymTable <$> get

createSymTable :: RunMonad ()
createSymTable = do
    oldState <- get
    put $ oldState { getSymTable = SymTable $ [] }

setSymTable :: SymTable -> RunMonad ()
setSymTable oldSymTable = do
    oldState <- get
    put $ oldState { getSymTable = oldSymTable }

runMonad f a = runWriterT (runStateT f a)
