module Var (Var(..), KeyWord(..), cons, parseScheme, toList, fromList) where

import Data.Tuple
import Data.Char
import qualified Data.Map as Map
import SExp hiding (cons, car, cdr, fromList, toList)
import qualified SExp (cons) 
import Debug.Trace


data KeyWord = Begin | Define | If | Lambda | Quote 
                deriving (Show, Eq, Ord)
keyWords = [(Begin, "begin"),
            (Define, "define"),
            (If, "if"),
            (Lambda, "lambda"),
            (Quote, "quote")]

keyWordToString = Map.fromList  keyWords
stringToKeyWord = Map.fromList $ map swap keyWords
  




data Var = VarBoolean {getBool :: Bool} |
           VarNull |
           VarPair {car :: Var, cdr :: Var} |
           VarSymbol {getSymbol :: String} |
           VarInt {getInt :: Int} | 
           VarChar {getChar :: Char} |
           VarString {getString :: String} | 
           VarKeyWord {getKeyWord :: KeyWord} |
           VarPrimProc {getPrimProc :: [Var] -> Var}
           

instance Show Var where
  show var = show $ varToSExp var

cons = VarPair                

toVar :: String -> Var
toVar "#t" = VarBoolean True
toVar "#T" = VarBoolean True
toVar "#f" = VarBoolean False
toVar "#F" = VarBoolean False
toVar "#\\Space" = VarChar ' '
toVar "#\\Tab" = VarChar '\t'
toVar "#\\Linefeed" = VarChar '\n'
toVar "#\\Return" = VarChar '\r'
toVar ('#':'\\':[x]) = VarChar x
toVar word@(x:_)
 |x=='\"' = VarString (read word)
 |isNumber x = VarInt (read word)
 |Map.member word stringToKeyWord = VarKeyWord (stringToKeyWord Map.! word)
 |otherwise = VarSymbol word

sexpToVar :: SExp String -> Var
sexpToVar (Atom x) = toVar x
sexpToVar (SList (Atom "\'":xs)) = trace "sfsdfsdfsd" $ fromList [VarKeyWord Quote, sexpToVar' xs]
sexpToVar (SList xs) = sexpToVar' xs

sexpToVar' :: [SExp String] -> Var
sexpToVar' [] = VarNull
sexpToVar' [Atom ".", x] = sexpToVar x
sexpToVar' (x:xs) = cons (sexpToVar x) (sexpToVar' xs)

varToSExp :: Var -> SExp String
varToSExp (VarPair a  d@(VarPair _ _)) = SExp.cons (varToSExp a) (varToSExp d)
varToSExp (VarPair a VarNull) = SList [varToSExp a]
varToSExp (VarPair a d) = SList [varToSExp a, Atom ".", varToSExp d]
varToSExp VarNull = SList []
varToSExp (VarBoolean True)  = Atom "#t"
varToSExp (VarBoolean False) = Atom "#f"
varToSExp (VarChar ' ') = Atom "#\\Space"
varToSExp (VarChar '\t') = Atom "#\\Tab"
varToSExp (VarChar '\n') = Atom "#\\Linefeed"
varToSExp (VarChar '\r') = Atom "#\\Return"
varToSExp (VarChar x) = Atom ("#\\" ++ [x])
varToSExp (VarSymbol word) = Atom word
varToSExp (VarString word) = Atom (show word)
varToSExp (VarInt num) = Atom (show num)
varToSExp (VarKeyWord key) = Atom (keyWordToString Map.! key)

toList :: Var -> [Var]
toList VarNull = []
toList (VarPair a d) = a:(toList d)

fromList :: [Var] -> Var
fromList = foldr cons VarNull 

parseScheme :: String -> Var
parseScheme = sexpToVar . parseSExp . lexSExp


