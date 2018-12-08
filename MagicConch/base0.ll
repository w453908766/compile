
@b = constant i32 5

define i32 @f(i32* %a){
entry:
  %a0 = load i32, i32* %a
  %b0 = load i32, i32* @b
  %add = add i32 %a0, %b0
  ret i32 %add
}


Module {

moduleDefinitions = [


GlobalDefinition (GlobalVariable {name = Name "b", isConstant = True, type' = IntegerType {typeBits = 32}, initializer = Just (Int {integerBits = 32, integerValue = 5})}),


GlobalDefinition (Function {returnType = IntegerType {typeBits = 32}, name = Name "f", 

  parameters = ([Parameter (PointerType {pointerReferent = IntegerType {typeBits = 32}}) (Name "a") []],False),  

  basicBlocks = [BasicBlock (Name "entry") [
    Name "a0" := Load {address = LocalReference (PointerType {pointerReferent = IntegerType {typeBits = 32}}) (Name "a")},
    Name "b0" := Load {address = ConstantOperand (GlobalReference (PointerType {pointerReferent = IntegerType {typeBits = 32}}) (Name "b"))},
    Name "add" := Add {operand0 = LocalReference (IntegerType {typeBits = 32}) (Name "a0"), operand1 = LocalReference (IntegerType {typeBits = 32}) (Name "b0")}] 
    (Do (Ret {returnOperand = Just (LocalReference (IntegerType {typeBits = 32}) (Name "add"))}))]})]}

