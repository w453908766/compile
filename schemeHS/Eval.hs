module Eval (eval, evalSequence) where

import Var

import PrimProc
import Control.Monad.State
import qualified Data.Map as Map


keyWordToFunc = Map.fromList [
                   (Begin, evalSequence),
                   (Define, evalDefine),
                   (If, evalIf),
                   (Lambda, evalLambda),
                   (Quote, evalQuote)]


type Env = [Map.Map String Var]

eval :: Var -> State Env Var
eval (VarPair (VarKeyWord keyWord) args) =
  func (toList args)
  where func = keyWordToFunc Map.! keyWord
  
eval funcCall@(VarPair _ _) = apply $ toList funcCall
eval (VarSymbol name) = findVar name
eval literal = return literal

evalSequence, evalDefine, evalIf, evalLambda, evalQuote :: [Var] -> State Env Var
evalSequence [exp] = eval exp
evalSequence (exp:last) = (eval exp) >> (evalBegin last)

evalDefine [VarSymbol name, value] = do
  (map0:last) <- get
  let map1 = Map.insert name value map0
  put (map1:last)
  return VarNull
  
evalIf [predicate, consequent, alternative] = do
  (VarBoolean pred) <- eval predicate
  if pred then eval consequent else eval alternative
  
evalLambda exp = do
  --env <- get
  --return $ cons exp env
  return VarNull
  
evalQuote [exp] = return exp

apply :: [Var] -> State Env Var
apply funcCall = do
  (func: args) <- traverse eval funcCall
  case func of
    (VarPrimProc f) -> return $ f args
    (VarPair lamb env) -> return $ VarNull

findVar :: String -> State Env Var
findVar name = do
  env <- get
  return $ findVar' env name

findVar' (map0:last) name = 
  case Map.lookup name map0 of
    Just result -> result
    Nothing -> findVar' last name


