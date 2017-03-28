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
import Data.Maybe   

data CursorStatus = On | Off deriving (Show,Eq)

data Val = BooleanVal Bool | VoidVal | NumberVal Double deriving (Show,Eq)

type Pos = (Double, Double)

type Segment = ((Double, Double),(Double, Double))

type Direction = Double

type VarDescript = (String,Val,Bool,OurType)

type FuncDescript = (String,[String],[InstrN],OurType)

data Cursor = Cursor {getPosition:: Pos, getDirection::Direction, getStatus::CursorStatus }

data Scope = Scope {getList::[VarDescript]}

data SymTable = SymTable {getScopes::[Scope]}

data OurState = OurState {getSymTable::SymTable, getCursor::Cursor,getFunDec::FuncDec, getMaxDown::Double, getMaxUp::Double, getMaxRight::Double,getMaxLeft::Double}

ourEmptyState = OurState (SymTable []) (Cursor (0,0) 0 Off) (FuncDec []) 0 0 0 0

data FuncDec = FuncDec { getDec:: [FuncDescript]}

type RunMonad a = StateT OurState (WriterT [Segment] IO ) a

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

thrd' :: (t,Val,t1,t2) -> Val
thrd' (_,v,_,_) = v

toRadian :: Double -> Double
toRadian num = num*pi/180 

lookInList :: String -> [VarDescript] -> Maybe VarDescript
lookInList s l = find ((==s).fst') l

lookInSymTable :: String -> RunMonad (VarDescript)
lookInSymTable s = fromJust.msum . map (lookInList s) . (map getList).getScopes.getSymTable <$> get

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

changeValInScope :: [VarDescript] -> String -> Val -> (Bool,[VarDescript])
changeValInScope scope string newVal = 
    foldl changeFun (False,[]) scope
        where changeFun (changed,list) (name,val,mutable,typeV) = case name==string of 
                        True -> (True,(name,newVal,mutable,typeV):list) 
                        _ -> (changed,(name,val,mutable,typeV):list) 

changeValInSymTable :: (String,Val) -> RunMonad ()
changeValInSymTable (string,newVal) = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        newScopes = snd $ foldl fun (False,[]) (getScopes oldSymTable)
        newSymTable = oldSymTable {getScopes = reverse newScopes}
    put $ oldState {getSymTable = newSymTable}
    where fun (changed,scopes) list = case changed of
                                        True -> (changed,list:scopes)
                                        _ -> case (changeValInScope (getList list) string newVal) of
                                                (True,changedScope) -> (True,Scope changedScope:scopes)
                                                (False,equalScope) ->  (changed,Scope equalScope:scopes)

getSymTable' :: RunMonad (SymTable)
getSymTable' = do
    getSymTable <$> get

createSymTable :: RunMonad ()
createSymTable = do
    oldState <- get
    put $ oldState { getSymTable = SymTable $ [] }

setSymTable :: SymTable -> RunMonad ()
setSymTable newSymTable = do
    oldState <- get
    put $ oldState { getSymTable = newSymTable }

---------------------------------------- FUNDEC -----------------------------------------

addToFunDec :: FuncDescript -> RunMonad ()
addToFunDec tuple = do
    oldState <- get
    let oldFunDec = getFunDec oldState
        newFunDec = FuncDec $ tuple:(getDec oldFunDec)
    put $ oldState { getFunDec = newFunDec }

findFunDec :: String -> RunMonad FuncDescript
findFunDec s = do
    fromJust.find ((==s).fst').getDec.getFunDec <$> get


---------------------------------------- CURSOR -----------------------------------------

onCursor ::  RunMonad () 
onCursor = do
    oldState <- get
    let oldCursor = getCursor oldState
        newCursor = oldCursor { getStatus = On }
    put $ oldState { getCursor = newCursor }

offCursor :: RunMonad () 
offCursor = do
    oldState <- get
    let oldCursor = getCursor oldState
        newCursor = oldCursor { getStatus = Off }
    put $ oldState { getCursor = newCursor }

forward :: Double -> RunMonad ()
forward steps = do
    oldState <- get
    let cursor = getCursor oldState
        (x,y) = getPosition cursor
        direc = getDirection cursor
        direction = toRadian direction 
        down = (getMaxDown oldState) `min` y `min` (y+sin direction)
        up = (getMaxUp oldState) `max` y `max` (y+sin direction)
        left = getMaxLeft oldState `min` x `min` (x+sin direction)
        right = getMaxRight oldState `max` x `max` (x+cos direction)
    case (getStatus cursor) of 
        Off -> return ()
        On -> tell [((x,y),(cos direction + x,sin direction + y))]
    put $ oldState { getMaxRight = right, getMaxLeft = left, getMaxUp = up, getMaxDown = down }
    setPosition (x+ cos direction,y+ sin direction)

backward :: Double -> RunMonad ()
backward steps = do
    oldState <- get
    let cursor = getCursor oldState
        (x,y) = getPosition cursor
        direc = getDirection cursor
        direction = toRadian direction
        down = (getMaxDown oldState) `min` y `min` (y+sin direction)
        up = (getMaxUp oldState) `max` y `max` (y+sin direction)
        left = getMaxLeft oldState `min` x `min` (x+sin direction)
        right = getMaxRight oldState `max` x `max` (x+cos direction)
    case (getStatus cursor) of 
        Off -> return ()
        On -> tell [((x,y),(x - cos direction,y - sin direction))]
    put $ oldState { getMaxRight = right, getMaxLeft = left, getMaxUp = up, getMaxDown = down }
    setPosition (x- cos direction,y- sin direction)

setPosition :: Pos -> RunMonad()
setPosition (x,y) = do
    oldState <- get
    let oldCursor = getCursor oldState
        newCursor = oldCursor { getPosition = (x,y) }
    put $ oldState { getCursor = newCursor } 


rotater :: Double -> RunMonad ()
rotater deegre = do
    oldState <- get
    let oldCursor = getCursor oldState
        oldDirection = getDirection oldCursor 
        newCursor = oldCursor {getDirection = oldDirection - deegre}
    put $ oldState { getCursor = newCursor }

rotatel :: Double -> RunMonad ()
rotatel deegre = do
    oldState <- get
    let oldCursor = getCursor oldState
        oldDirection = getDirection oldCursor 
        newCursor = oldCursor {getDirection = oldDirection + deegre}
    put $ oldState { getCursor = newCursor }



runMonad f a = runWriterT (runStateT f a)
