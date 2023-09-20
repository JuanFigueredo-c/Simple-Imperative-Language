module Eval2
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
-- Completar la definición
initState :: State
initState = undefined

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = undefined

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = undefined

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm = undefined

-- Evalua una expresion
-- Completar la definición

combineExp :: Exp a -> Exp a -> State -> (a -> a -> b) -> Either Error (Pair b State)
combineExp e1 e2 s op =  case evalExp e1 s of
                         Left error -> Left error
                         Right (n1 :!: s') -> case evalExp e2 s' of
                                              Left error -> Left error
                                              Right  (n2 :!: s'') -> Right (op n1 n2) :!: s''
                          
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = n :!: s
evalExp (Var v) s = lookfor v s :!: s
evalExp (UMinus e) s = case evalExp e s of
                       Left error -> Left error
                       Right (-n :!: s') -> Right (-n :!: s')
evalExp (Plus e1 e2) s = combineExp e1 e2 s (+) 
evalExp (Times e1 e2) s = combineExp e1 e2 s (*)                                 
evalExp (Div e1 e2) s = combineExp e1 e2 s (\x y -> if y == 0 then 
evalExp (EAssgn v e) s = case evalExp e s of
                         Left error -> Left error
                         Right (n :!: (update v n s')) -> Right (n :!: (update v n s'))
evalExp (ESeq e1 e2) s = case evalExp e1 s of
                         Left error -> Left error
                         Right (_ :!: s') -> Right evalExp e2 s'
evalExp BTrue s = True :!: s
evalExp BFalse s = False :!: s
evalExp (Lt e1 e2) s = combineExp e1 e2 s  (<)
evalExp (Gt e1 e2) s = combineExp e1 e2 s (>)
evalExp (Eq e1 e2) s = combineExp e1 e2 s (==)
evalExp (NEq e1 e2) s = combineExp e1 e2 s (/=)
evalExp (And e1 e2) s = combineExp e1 e2 s (&&)
evalExp (Or e1 e2) s = combineExp e1 e2 s (||)
evalExp (Not e) s = case evalExp e s
                    in (not b) :!: s'
