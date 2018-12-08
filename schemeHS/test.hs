import System.IO

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short.Internal as SBS
import Data.Functor
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Monoid
import Foreign.Ptr
import Data.Word

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

toASSM :: LLVM.AST.Module -> IO BS.ByteString
toASSM ast = withContext $ \context -> withModuleFromAST context ast moduleLLVMAssembly              

toAST :: Modu.LLVMAssemblyInput s => s -> IO LLVM.AST.Module
toAST ass = withContext $ \context -> withModuleFromLLVMAssembly context ass moduleAST

{-
ttt = Module {moduleName = "<string>", 
              moduleSourceFileName = "<string>", 
              moduleDataLayout = Nothing, 
              moduleTargetTriple = Nothing, 
              moduleDefinitions = [
                GlobalDefinition (Function {linkage = External, visibility = Default, dllStorageClass = Nothing, callingConvention = C, returnAttributes = [], returnType = IntegerType {typeBits = 32}, name = Name "f", parameters = ([],False), functionAttributes = [], section = Nothing, comdat = Nothing, alignment = 0, garbageCollectorName = Nothing, prefix = Nothing, basicBlocks = [BasicBlock (UnName 0) [] (Do (Ret {returnOperand = Just (ConstantOperand (Int {integerBits = 32, integerValue = 5})), metadata' = []}))], personalityFunction = Nothing})
                ]}
-}

fact = Module (toSBS "fact") (toSBS "base.ll") Nothing Nothing [
            GlobalDefinition $ functionDefaults {
             returnType = IntegerType {typeBits = 32}, 
             name = Name (toSBS "fact"), 
             parameters = ([Parameter (IntegerType {typeBits = 32}) (UnName 0) []],False), 
             basicBlocks = [
               BasicBlock 
                 (UnName 1) 
                 [UnName 2 := ICmp {iPredicate = IPred.EQ, operand0 = LocalReference (IntegerType {typeBits = 32}) (UnName 0), operand1 = ConstantOperand (C.Int 32 0), metadata = []}] 
                 (Do (CondBr {condition = LocalReference (IntegerType {typeBits = 1}) (UnName 2), trueDest = UnName 10, falseDest = UnName 3, metadata' = []})),
               BasicBlock 
                 (UnName 3) 
                 [] 
                 (Do (Br {dest = UnName 4, metadata' = []})),
               BasicBlock 
                 (UnName 4) 
                 [
                   UnName 5 := Phi (IntegerType 32) [(LocalReference (IntegerType {typeBits = 32}) (UnName 7),UnName 4),(LocalReference (IntegerType {typeBits = 32}) (UnName 0),UnName 3)] [],
                   UnName 6 := Phi (IntegerType 32) [(LocalReference (IntegerType {typeBits = 32}) (UnName 8),UnName 4),(ConstantOperand (C.Int 32 1),UnName 3)] [],
                   UnName 7 := Add {nsw = True, nuw = False, operand0 = LocalReference (IntegerType {typeBits = 32}) (UnName 5), operand1 = ConstantOperand (C.Int 32 4294967295), metadata = []},
                   UnName 8 := Mul {nsw = True, nuw = False, operand0 = LocalReference (IntegerType {typeBits = 32}) (UnName 5), operand1 = LocalReference (IntegerType {typeBits = 32}) (UnName 6), metadata = []},
                   UnName 9 := ICmp {iPredicate = IPred.EQ, operand0 = LocalReference (IntegerType {typeBits = 32}) (UnName 7), operand1 = ConstantOperand (C.Int 32 0), metadata = []}] 
                 (Do (CondBr {condition = LocalReference (IntegerType {typeBits = 1}) (UnName 9), trueDest = UnName 10, falseDest = UnName 4, metadata' = []})),
               BasicBlock 
                 (UnName 10) 
                 [UnName 11 := Phi (IntegerType 32) [(ConstantOperand (C.Int 32 1),UnName 1),(LocalReference (IntegerType {typeBits = 32}) (UnName 8),UnName 4)] [] ] 
                 (Do (Ret {returnOperand = Just (LocalReference (IntegerType {typeBits = 32}) (UnName 11)), metadata' = []}))] 
             }
 ] 



ast0 = Module (toSBS "module0") (toSBS "file0") Nothing Nothing [
          GlobalDefinition $ globalVariableDefaults {
            G.name = Name (toSBS "ccc"),
            G.type' = i32,
            G.isConstant = True,
            G.initializer = Just $ C.Int 32 42
          },
          GlobalDefinition $ functionDefaults {
            G.returnType = i32,
            G.name = UnName 0,
            G.basicBlocks = [
              BasicBlock (UnName 1) [
              
                UnName 2 := GetElementPtr {
                  inBounds = True,
                  address = ConstantOperand (C.GlobalReference (ptr i32) (Name (toSBS "ccc"))),
                  indices = [ ConstantOperand (C.Int 32 0) ],
                  metadata = []
                },
                UnName 3 := Load {
                  volatile = False,
                  address = LocalReference (ptr i32) (UnName 2),
                  maybeAtomicity = Nothing,
                  LLVM.AST.alignment = 1,
                  metadata = []
                }
              ] (
                Do $ Ret (Just (LocalReference i32 (UnName 3))) []
              )
             ]
           }
          ]


ass1 = "; ModuleID = '<string>'\n\
               \source_filename = \"<string>\"\n\
               \\n\
               \@0 = constant i32 42\n\
               \\n\
               \define i32 @1() {\n\
               \  %1 = load i32, i32* @0, align 1\n\
               \  ret i32 %1\n\
               \}\n"

simple = do
  ass0 <- toASSM ast0
  putStr $ fromBS ass0
  putStr "---------------------------------------\n"
  ast1 <- toAST ass1
  print ast1
  putStr "---------------------------------------\n"



main :: IO ()
main = do
  handle <- openFile "base0.ll" ReadMode
  contents <- hGetContents handle
  ast <- toAST contents
  print ast
  hClose handle
