module PrimProc (primProcMap) where
import MyLib
import Var
import qualified Data.Map as Map


funcCons, funcCar, funcCdr, 
  funcAdd, funcSub, funcMul, funcDiv,
  funcLess,funcMore,
  funcAnd,funcOr,funcNot :: [Var] -> Var

funcCons [a,d] = cons a d
funcCar [z] = car z
funcCdr [z] = cdr z

funcAdd = VarInt . sum . (map getInt)
funcSub = VarInt . (foldl1 (-)) . (map getInt)
funcMul = VarInt . product . (map getInt)
funcDiv = VarInt . (foldl1 div) . (map getInt)

funcLess = VarBoolean . and . (cross2 (<)) . (map getInt)
funcMore = VarBoolean . and . (cross2 (>)) . (map getInt)

--funcEq

funcAnd = VarBoolean . and . (map getBool)
funcOr  = VarBoolean . or  . (map getBool)
funcNot = VarBoolean . not . getBool . head

primProcName = ["cons", "car", "cdr", 
                "+", "-", "*", "quotient", 
                "<", ">", 
                "and", "or", "not"]
primProcs = [funcCons, funcCar, funcCdr, 
             funcAdd, funcSub, funcMul, funcDiv,
             funcLess,funcMore,
             funcAnd,funcOr,funcNot]


primProcMap = Map.fromList $ zip primProcName (map VarPrimProc primProcs)



