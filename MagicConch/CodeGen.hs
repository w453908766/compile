module CodeGen where

import Var
import System.IO

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short.Internal as SBS
import Data.Functor
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Monoid
import Control.Monad.State
import Foreign.Ptr
import Data.Word
import qualified Data.Map as Map

import LLVM.Context
import LLVM.Module
import LLVM.Diagnostic
import LLVM.AST
import LLVM.AST.Type as A.T
import LLVM.AST.Name
import LLVM.AST.AddrSpace
import LLVM.AST.Global
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPPred
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.RMWOperation as RMWOp
import qualified LLVM.Internal.Module as Modu

toBS :: String -> BS.ByteString
toBS = BS.pack . map (fromIntegral . fromEnum)
 
fromBS :: BS.ByteString -> String
fromBS = map (toEnum . fromIntegral) . BS.unpack

toSBS :: String -> SBS.ShortByteString
toSBS = SBS.pack . map (fromIntegral . fromEnum)
 
fromSBS :: SBS.ShortByteString -> String
fromSBS = map (toEnum . fromIntegral) . SBS.unpack

toASS :: LLVM.AST.Module -> IO BS.ByteString
toASS ast = withContext $ \context -> withModuleFromAST context ast moduleLLVMAssembly              

toAST :: Modu.LLVMAssemblyInput s => s -> IO LLVM.AST.Module
toAST ass = withContext $ \context -> withModuleFromLLVMAssembly context ass moduleAST


type Env a = State (Map.Map Name Operand) a

makeConstant :: Var -> C.Constant
makeConstant (VarBoolean b) = C.Int 1 $ toInteger $ fromEnum b
makeConstant (VarChar c) = C.Int 8 $ toInteger $ fromEnum c
makeConstant (VarInt i) = C.Int 32 $ toInteger i


makeType :: Var -> Type
makeType (VarSymbol "Bool") = i1
makeType (VarSymbol "Char") = i8
makeType (VarSymbol "Int") = i32
makeType (VarList (retType:paramTypes)) = 
  FunctionType (makeType retType) (map makeType paramTypes) False

getOpType :: Operand -> Type
getOpType (LocalReference ty _) = ty
getOpType (ConstantOperand (C.GlobalReference ty _)) = ty


makeCall :: String -> [Operand] -> (Name, Instruction, Type)
makeCall "+" [op0, op1] = (mkName "add", Add False False op0 op1 [], getOpType op0)


makeInst :: Var -> Map.Map Name Operand -> State [Named Instruction] Operand

makeInst b@(VarBoolean _) _ = return $ ConstantOperand $ makeConstant b
makeInst b@(VarChar _) _ = return $ ConstantOperand $ makeConstant b
makeInst b@(VarInt _) _ = return $ ConstantOperand $ makeConstant b

makeInst (VarSymbol name) env = do
  return $ (Map.!) env (mkName name)

makeInst (VarList ((VarSymbol func) : args)) env = do
  args' <- mapM (flip makeInst env) args
  let (name, inst, ty) =  makeCall func  args'
  
  modify ((name := inst) :)

  return $ LocalReference ty name





makeBasicBlocks :: Var -> [BasicBlock] -> Env [BasicBlock]
makeBasicBlocks (VarList (VarSymbol "if") pred conq alt) 
makeBasicBlocks expr bbs = do
  env <- get
  let (op, insts) = runState (makeInst expr env) []
  let insts' = reverse insts
  return $ (BasicBlock (mkName "return") insts' (Do $ Ret (Just op) [])) : bbs
  

makeBasicBlocks :: Var -> Env [BasicBlock]
makeBasicBlocks var = makeBasicBlocks' var 



addParam :: (Type, Name) -> Env Parameter
addParam (paramType, paramName) = do
  modify (Map.insert paramName (LocalReference paramType paramName))
  return $ Parameter paramType paramName []

makeDeclare :: Var -> Var -> Var -> Env Global
makeDeclare funcType (VarList head) body = do
  let (funcName:paramNames) = map (\(VarSymbol name) -> mkName name) head 
  let FunctionType retType paramsType isVA = makeType funcType
  globalEnv <- get
  params <- mapM addParam (zip paramsType paramNames)

  bbs <- makeBasicBlocks body []
  
  put globalEnv
  return $ functionDefaults {
    G.name = funcName, 
    returnType = retType, 
    parameters = (params, False),
    basicBlocks = bbs
  }

makeDeclare atomType (VarSymbol name) init = 
  return $ globalVariableDefaults {
    G.name = mkName name, 
    G.type' = makeType atomType, 
    G.isConstant = True,
    G.initializer = Just $ makeConstant init
  }

  



makeGlobalDeclare :: Var -> Env Definition
makeGlobalDeclare (VarList [VarSymbol "define", types, head, body]) = do
  global <- makeDeclare types head body
  let operand = ConstantOperand $ C.GlobalReference (G.type' global) (G.name global)
  modify (Map.insert (G.name global) operand) 
  return $ GlobalDefinition global 

  

makeModule :: Var -> LLVM.AST.Module
makeModule (VarList globals) = 
  Module (toSBS "module") (toSBS "file") Nothing Nothing defines
  where
    (defines, operandMap) = runState (mapM makeGlobalDeclare globals) Map.empty
