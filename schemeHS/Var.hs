import qualified Data.Map as Map
import SExp hiding (cons, car, cdr, fromList, toList)
import qualified SExp (cons) 
import Debug.Trace



data Var = VarBoolean {getBool :: Bool} |
           VarChar {getChar :: Char} |  
           VarInt {getInt :: Int} | 
           VarSymbol {getSymbol :: String} |
           VarList {getList :: [Var]}
           deriving (Show, Eq, Ord)
           

toVar :: SExp String -> Var
toVar (SList xs) = VarList xs
toVar (Atom "#t") = VarBoolean True
toVar (Atom "#f") = VarBoolean False
toVar (Atom word@(x:_))
 |x=='\'' = VarChar (read word)
 |x=='\"' = VarList (read word)
 |isNumber x = VarInt (read word)
 |otherwise = VarSymbol word


fromVar :: Var -> SExp String
fromVar (VarList xs) = SList xs
fromVar (VarBoolean True)  = Atom "#t"
fromVar (VarBoolean False) = Atom "#f"
fromVar (VarChar x) = Atom (show x)
fromVar (VarInt x) = Atom (show x)
fromVar (VarSymbol x) = Atom x

-- (: distance (-> pt pt Real))
toLLVM :: Var -> LLVM.AST.Module
toLLVM (VarList [VarSymbol ":", VarSymbol x, 
