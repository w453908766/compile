module Sema where

import Var
import Control.Monad
import qualified Data.Map as Map

{-
pushSymbolTable :: Var -> Map.Map String Var
pushSymbolTable (VarList global) = Map.fromList $ do
  VarList [VarSymbol de, sym, val] <- global
  guard (de == "declare")
  let (VarSymbol name) = sym
  return (name, val)
 -}


 


