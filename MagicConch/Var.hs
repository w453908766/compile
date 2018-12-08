module Var where


import qualified Data.Map as Map
import SExp
import qualified Data.Char as Char
import Debug.Trace



data Var = VarBoolean Bool |
           VarChar Char |
           VarInt Int |
           VarSymbol String |
           VarList [Var]
           deriving (Show, Eq, Ord)
           

toVar :: SExp String -> Var
toVar (SList xs) = VarList (map toVar xs)
toVar (Atom "True") = VarBoolean True
toVar (Atom "False") = VarBoolean False
toVar (Atom word@(x:_))
 |x=='\'' = VarChar (read word)
 |x=='\"' = VarList (map VarChar ((read word) :: String))
 |Char.isNumber x = VarInt (read word)
 |otherwise = VarSymbol word


fromVar :: Var -> SExp String
fromVar (VarList xs) = SList (map fromVar xs)
fromVar (VarBoolean True)  = Atom "True"
fromVar (VarBoolean False) = Atom "False"
fromVar (VarChar x) = Atom (show x)
fromVar (VarInt x) = Atom (show x)
fromVar (VarSymbol x) = Atom x

-- (: distance (-> pt pt Real))
-- toLLVM :: Var -> LLVM.AST.Module
-- toLLVM (VarList [VarSymbol ":", VarSymbol x, 
