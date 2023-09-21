module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s
stepComm (Let v e) s =  let (n :!: s') = evalExp e s
                        in Skip :!: (update v n s')
stepComm (IfThenElse e c1 c2) s = let (b :!: s') = evalExp e s
                                  in if b then c1 :!: s' else c2 :!: s'
stepComm (Seq Skip c2) s = c2 :!: s
stepComm (Seq c1 c2)   s =  let (c1' :!: s') = stepComm c1 s
                            in (Seq c1' c2) :!: s'
stepComm r@(Repeat c e) s = (Seq c (IfThenElse e Skip r)) :!: s

-- Evalua una expresion
-- Completar la definición

combineExp :: Exp a -> Exp a -> State -> (a -> a -> b) -> Pair b State
combineExp e1 e2 s op = let (n1 :!: s') = evalExp e1 s
                            (n2 :!: s'') = evalExp e2 s'
                        in (op n1 n2) :!: s''

evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = n :!: s
evalExp (Var v) s = lookfor v s :!: s
evalExp (UMinus e) s = let (n :!: s') = evalExp e s in -n :!: s'
evalExp (Plus e1 e2) s = combineExp e1 e2 s (+)
evalExp (Minus e1 e2) s = combineExp e1 e2 s (-)
evalExp (Times e1 e2) s = combineExp e1 e2 s (*)
evalExp (Div e1 e2) s = combineExp e1 e2 s div
evalExp (EAssgn v e) s = let (n :!: s') = evalExp e s in n :!: (update v n s')
evalExp (ESeq e1 e2) s = let (_ :!: s') = evalExp e1 s in evalExp e2 s'
evalExp BTrue s = True :!: s
evalExp BFalse s = False :!: s
evalExp (Lt e1 e2) s = combineExp e1 e2 s (<)
evalExp (Gt e1 e2) s = combineExp e1 e2 s (>)
evalExp (Eq e1 e2) s = combineExp e1 e2 s (==)
evalExp (NEq e1 e2) s = combineExp e1 e2 s (/=)
evalExp (And e1 e2) s = combineExp e1 e2 s (&&)
evalExp (Or e1 e2) s = combineExp e1 e2 s (||)
evalExp (Not e) s = let (b :!: s') = evalExp e s
                    in (not b) :!: s'
