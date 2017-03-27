
module RunAST where

import RunMonad
import AST


runConstrN :: ConstrN -> RunMonad ()
runConstrN PN ldfn iln = do
    --declarar funciones predefinidas
    runConstrN ldfn
    runInstrListN iln

runConstrN (LDFN l) = do
    mapM_ runFuncDefN lfdn

runConstrN (LDN l) = do
    mapM_ runTypeAndList l
    where 
        runTypeAndList :: (TypeN, [VarN]) -> RunMonad ()
        runTypeAndList (tN, varNList) = do
            let t = typeConvert tN
            mapM_ (adder t) varNList
        adder :: OurType -> VarN -> RunMonad ()
        adder t varN = do
            let (s, me) = case varN of
                (VarN st) -> (st, Nothing)
                (VarValN st e) -> (st, Just e)
            -- ...