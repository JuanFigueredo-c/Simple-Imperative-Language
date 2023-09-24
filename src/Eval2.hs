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
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of 
              Just val -> Right val
              Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

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
stepComm Skip s = return (Skip :!: s)
stepComm (Let v e) s =  do (n :!: s') <- evalExp e s
                           return $ (Skip :!: (update v n s'))
stepComm (IfThenElse e c1 c2) s = do (b :!: s') <- evalExp e s
                                     return $ if b then (c1 :!: s') else (c2 :!: s')
stepComm (Seq Skip c2) s = return $ (c2 :!: s)
stepComm (Seq c1 c2)   s =  do (c1' :!: s') <- stepComm c1 s 
                               return $ ((Seq c1' c2) :!: s')
stepComm r@(Repeat c e) s = return $ (Seq c (IfThenElse e Skip r)) :!: s
                            

-- Evalua una expresion
-- Completar la definición

rightBinOp :: (a -> a -> b) -> (a -> a -> Either Error b)
rightBinOp op = (\x y -> Right (op x y)) 

combineExp :: Exp a -> Exp a -> State -> (a -> a -> Either Error b) -> Either Error (Pair b State)
combineExp e1 e2 s op = do (n1 :!: s') <- evalExp e1 s
                           (n2 :!: s'') <- evalExp e2 s
                           val <- op n1 n2 
                           return $ (val :!: s'') 

evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var v) s = case lookfor v s of 
                    Left error -> Left error
                    Right n -> Right (n :!: s)
evalExp (UMinus e) s = case evalExp e s of
                       Left error -> Left error
                       Right (n :!: s') -> Right (-n :!: s')
evalExp (Plus e1 e2) s = combineExp e1 e2 s (rightBinOp (+))
evalExp (Minus e1 e2) s = combineExp e1 e2 s (rightBinOp (-))
evalExp (Times e1 e2) s = combineExp e1 e2 s (rightBinOp (*))                                 
evalExp (Div e1 e2) s = combineExp e1 e2 s (\x y -> if y == 0 then (Left DivByZero) else Right (div x y)) 
evalExp (EAssgn v e) s = case evalExp e s of
                         Left error -> Left error
                         Right (n :!: s') -> Right (n :!: (update v n s'))
evalExp (ESeq e1 e2) s = case evalExp e1 s of
                         Left error -> Left error
                         Right (_ :!: s') -> evalExp e2 s'
evalExp BTrue s =  Right (True :!: s)
evalExp BFalse s = Right (False :!: s)
evalExp (Lt e1 e2) s = combineExp e1 e2 s  (rightBinOp (<))
evalExp (Gt e1 e2) s = combineExp e1 e2 s (rightBinOp (>))
evalExp (Eq e1 e2) s = combineExp e1 e2 s (rightBinOp (==))
evalExp (NEq e1 e2) s = combineExp e1 e2 s (rightBinOp (/=))
evalExp (And e1 e2) s = combineExp e1 e2 s (rightBinOp (&&))
evalExp (Or e1 e2) s = combineExp e1 e2 s (rightBinOp (||))
evalExp (Not e) s = case evalExp e s of
                    Left error -> Left error
                    Right (b :!: s') -> Right ((not b) :!: s')
