-- Retina - Proyecto de Traductores
-- OurMonad, Monad para realizar el analisis de contexto y sus funciones respectivas.
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module RunMonad where

import AST
import OurType
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Either
import Data.List

data CursorStatus = On | Off deriving (Show,Eq)

data Val = BooleanVal Bool | NumberVal Double deriving (Show,Eq)

type Pos = (Double, Double)

type Direction = Double

type VarDescript = (String,Val,Bool,OurType)

type FuncDescript = (String,[String],[InstrN],OurType)

data Cursor = Cursor {getPos:: Pos, getDirection::Direction, getStatus::CursorStatus }

data Scope = Scope {getList::[VarDescript]}

data SymTable = SymTable {getScopes::[Scope]}

data OurState = OurState {getSymTable::SymTable, getCursor::Cursor,getFunDec::FuncDec}

data FuncDec = FuncDec { getDec:: [FuncDescript]}

type RunMonad a = StateT OurState (WriterT [Int] IO ) a

addToSymTable :: VarDescript -> RunMonad ()
addToSymTable tuple = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopeList = getScopes oldSymTable
        newLastScope = Scope $ tuple:(getList $ head oldScopeList)
        newScopeList = newLastScope:(tail oldScopeList)
        newSymTable = oldSymTable { getScopes = newScopeList }
    put $ oldState { getSymTable = newSymTable }

fst' :: (String,t,t1,t2) -> String
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

---------------------------------------- FUNDEC -----------------------------------------

addToFundec :: FuncDescript -> RunMonad ()
addToFundec tuple = do
    oldState <- get
    let oldFunDec = getFunDec oldState
        newFunDec = FuncDec $ tuple:(getDec oldFunDec)
    put $ oldState { getFunDec = newFunDec }

findFundec :: String -> RunMonad (Maybe FuncDescript)
findFundec s = do
    find ((==s).fst').getDec.getFunDec <$> get


---------------------------------------- CURSOR -----------------------------------------

onCursor :: CursorStatus -> RunMonad () 
onCursor status = do
    oldState <- get
    let oldCursor = getCursor oldState
        newCursor = oldCursor { getStatus = On }
    put $ oldState { getCursor = newCursor }

offCursor :: CursorStatus -> RunMonad () 
offCursor status = do
    oldState <- get
    let oldCursor = getCursor oldState
        newCursor = oldCursor { getStatus = Off }
    put $ oldState { getCursor = newCursor }

runMonad f a = runWriterT (runStateT f a)
