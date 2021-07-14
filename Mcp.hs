module Mcp where
import Parser
import Lexer
import Interm

start :: [FunIR] -> [String]
start xs = init1 (reverse xs)

init1 :: [FunIR] -> [String]
init1 [] = ["print_int:" ++ "\n" ++ " li $v0, 1" ++ "\n" ++ " syscall" ++ "\n" ++ " jr $ra" ++ "\n" ++ " scan_int:" ++ "\n" ++ " li $v0, 5" ++ "\n" ++ " syscall" ++ "\n" ++ " jr $ra"]
init1 (x:xs) = begin x ++ init1 xs

begin ::  FunIR -> [String]
begin (FunIR "main" temp instr) = ("main" ++ ":") : "\n" : getInstr instr ++ ["li $v0, 10" ++ "\n" ++ " syscall" ++ "\n"]
begin (FunIR label temp instr) = (label ++ ":") : "\n" : getInstr instr

getInstr:: [Instr] -> [String]
getInstr [] = []

getInstr ((COND x Beq y labelt labelf) : LABEL labelt' : rest) 
    | labelt == labelt' = (generateCodeRelOp "bne" x y labelf) : "\n" : (labelt' ++ ":") : "\n" : getInstr rest

getInstr ((COND x Bne y labelt labelf) : LABEL labelt' : rest) 
    | labelt == labelt' = (generateCodeRelOp "beq" x y labelf) : "\n" : (labelt' ++ ":") : "\n" : getInstr rest

getInstr ((COND x Blt y labelt labelf) : LABEL labelt' : rest) 
    | labelt == labelt' = (generateCodeRelOp "bge" x y labelf) : "\n" : (labelt' ++ ":") : "\n" : getInstr rest

getInstr ((COND x Ble y labelt labelf) : LABEL labelt' : rest) 
    | labelt == labelt' = (generateCodeRelOp "bgt" x y labelf) : "\n" : (labelt' ++ ":") : "\n" : getInstr rest

getInstr ((COND x Bgt y labelt labelf) : LABEL labelt' : rest) 
    | labelt == labelt' = (generateCodeRelOp "ble" x y labelf) : "\n" : (labelt' ++ ":") : "\n" : getInstr rest

getInstr ((COND x Bge y labelt labelf) : LABEL labelt' : rest) 
    | labelt == labelt' = (generateCodeRelOp "blt" x y labelf) : "\n" : (labelt' ++ ":") : "\n" : getInstr rest

getInstr ((RETURNT x) : rest) = (createReturn x) : "\n" : getInstr rest

getInstr ((CALL x label args) : rest) = (loadArgs args) : "\n" : (alloc 1) : "\n" : (callLabel label) : "\n" : (unalloc 1) : "\n" : (loadReturned x) : "\n" : getInstr rest

getInstr ((JUMP x) : rest) = ("j " ++ x) : "\n" : getInstr rest

getInstr ((LABEL x) : rest) = (x ++ ":") : "\n" : getInstr rest

getInstr ((MOVE x y) : rest) = ("move $" ++ x ++ ", $" ++ y) : "\n" : getInstr rest
getInstr ((MOVEI x y) : rest) = ("li $" ++ x ++ ", " ++ show y) : "\n" : getInstr rest
getInstr ((OP Add x y z) : rest) = (generateCodeOp "add" x y z) : "\n" : getInstr rest
getInstr ((OP Minus x y z) : rest) = (generateCodeOp "sub" x y z) : "\n" : getInstr rest
getInstr ((OP Mult x y z) : rest) = (generateCodeOp "mul" x y z) : "\n" : getInstr rest
getInstr ((OP Div x y z) : rest) = (generateCodeDiv x y z) : "\n" : getInstr rest 
getInstr ((OP Mod x y z) : rest) = (generateCodeMod x y z) : "\n" : getInstr rest

generateCodeDiv :: Temp -> Temp -> Temp -> String
generateCodeDiv x y z = "div $" ++ y ++ ", $" ++ z ++ "\n" ++ "mflo $" ++ x

generateCodeMod :: Temp -> Temp -> Temp -> String
generateCodeMod x y z = "div $" ++ y ++ ", $" ++ z ++ "\n" ++ "mfhi $" ++ x


generateCodeRelOp :: String -> Temp -> Temp -> Label -> String 
generateCodeRelOp cmd t0 t1 labelf = cmd ++ " $" ++ t0 ++ ", " ++ "$" ++ t1 ++ ", " ++ labelf

generateCodeOp :: String -> Temp -> Temp -> Temp -> String
generateCodeOp cmd t0 t1 t2 = cmd ++ " $" ++ t0 ++ ", " ++ "$" ++ t1 ++ ", " ++ "$" ++ t2

alloc :: Int -> String
alloc x = "addiu $sp, $sp, -8 " ++ "\n" ++ " sw $ra, 0($sp)" ++ "\n" ++ " sw $fp, 4($sp)"

unalloc :: Int -> String
unalloc x = "lw $fp, 4($sp) " ++ "\n" ++ " lw $ra, 0($sp)" ++ "\n" ++ " addiu $sp, $sp, 8"

createReturn :: Temp -> String
createReturn x = "move $v0, " ++ "$" ++ x ++ "\n" ++ " jr $ra"

loadReturned :: Temp -> String
loadReturned x = ("move $" ++ x ++ ", $v0")

loadArgs :: [Temp] -> String
ĺoadArgs [] = ""
loadArgs (x:xs) = "move $a0, $" ++ x ++ loadArgs1 xs
loadArgs _ = ""

loadArgs1 :: [Temp] -> String
loadArgs1 [] = ""
loadArgs1 (x:xs) = "\n" ++ " move $a1, $" ++ x ++ loadArgs2 xs

loadArgs2 :: [Temp] -> String
loadArgs2 [] = ""
loadArgs2 (x:xs) = "\n" ++ "move $a2, $" ++ x ++ loadArgs3 xs

loadArgs3 :: [Temp] -> String
loadArgs3 [] = ""
loadArgs3 (x:xs) = "\n" ++ "move $a3, $" ++ x


callLabel :: Label -> String
callLabel label = "jal " ++ label

