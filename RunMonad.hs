-- Retina - Proyecto de Traductores
-- OurMonad, Monad para realizar el analisis de contexto y sus funciones respectivas.
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

module RunMonad where

import AST
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Either
import Data.List

data CursorStatus = On | Off deriving (Show,Eq)

data Val = Boolean Bool | Number Double deriving (Show,Eq)

type Pos = (Double, Double)

type Direction = Double

data Cursor = Cursor {getPos:: Pos, getDirection::Direction, getStatus::CursorStatus }

data Scope = Scope {getList::[(String,Val)]}

data SymTable = SymTable {getScopes::[Scope]}

data OurState = OurState {getSymTable::SymTable, getCursor::Cursor}

data FuncDec = FuncDec{ getDec:: [(String,[String],[InstrN])]}

type RunMonad a = StateT OurState (WriterT [Int] IO ) a

addToSymTable :: (String, Val) -> RunMonad ()
addToSymTable pair = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldScopeList = getScopes oldSymTable
        newLastScope = Scope $ pair:(getList $ head oldScopeList)
        newScopeList = newLastScope:(tail oldScopeList)
        newSymTable = oldSymTable { getScopes = newScopeList }
    put $ oldState { getSymTable = newSymTable }

runMonad f a = runWriterT (runStateT f a)
