module Interm where
--import AST
import Parser
import Lexer
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.List

type Ident = String
type Temp= String
type Label= String
type Table = Map Ident String


--contador de temporarios e etiquetas
type Count = (Int, Int)

newTemp :: State Count Temp
newTemp  = do (temps, labels) <- get
              put (temps+1, labels)
              return ("t" ++ show temps)

newLabel :: State Count Label
newLabel = do (temps, labels) <- get
              put (temps, labels+1)
              return ("L" ++ show labels)

data FunIR = FunIR Label [Temp] [Instr]
          deriving Show

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOp Temp Temp Temp
           | OPI BinOp Temp Ident Int
           | LABEL Label
           | JUMP Label
           | COND Temp RelOp Temp Label Label
           | CALL Temp Label [Temp] -- destino funcao argumentos
           | RETURNT Temp
           deriving Show

--translate Expessions
transExp :: Table -> Exp -> Temp -> State Count [Instr]

transExp tabl (Id x) dest
  = case Map.lookup x tabl of
    Just temp -> return [MOVE dest temp]
    Nothing -> error "invalid variable"

transExp tabl (Bool n) dest
   = case n of
     True -> return [MOVEI dest 1]
     otherwise -> return [MOVEI dest 0]

transExp tabl (Num n) dest
    = return [MOVEI dest n]

transExp tabl (Op op e1 e2) dest
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl e1 temp1
         code2 <- transExp tabl e2 temp2
         popTemp
         popTemp
         return (code1 ++ code2 ++[OP op dest temp1 temp2])

transExp tabl (BtweenParent e1) dest
      =  do code <- transExp tabl e1 dest
            popTemp
            return code

--para chamada funcoes : CALL Temp Label [Temp]
transExp tabl (CallFunct f args) dest
  =  do  temps <- sequence [transParams tabl a | a<- args]
         return ([CALL dest f temps])



transParams tabl (Id x) -- x,y
 = case Map.lookup x tabl of
   Just temp -> return temp
   Nothing -> error ("invalid variable"++ show x)

-- translate  assignments
transAssign :: Table -> Assign -> State Count [Instr]
transAssign tabl (ExpA id e2)
      = case Map.lookup id tabl of
        Nothing -> error ("undefined variable" ++ show id)
        Just temp1 -> do temp2 <- newTemp
                         code <- transExp tabl e2 temp2
                         popTemp
                         return (code ++[MOVE temp1 temp2])

-- translate statments
transStm :: Table -> Stm -> State Count [Instr]

transStm tabl (If comp stm1)
  = do ltrue <- newLabel
       lfalse <- newLabel
       code0 <- transCond tabl comp ltrue lfalse
       code1 <-  transStm tabl stm1
       return (code0 ++ [LABEL ltrue] ++ code1 ++[LABEL lfalse])


transStm tabl (IfElse comp stm1 stm2)
  =do ltrue <- newLabel
      lfalse <- newLabel
      lend <- newLabel
      code0 <- transCond tabl comp ltrue lfalse
      code1 <- transStm tabl stm1
      code2 <- transStm tabl stm2
      return (code0 ++ [LABEL ltrue] ++ code1 ++
               [JUMP lend, LABEL lfalse] ++ code2 ++[LABEL lend])

transStm tabl (IfE e1 stm1)
   = do ltrue <- newLabel
        lfalse <- newLabel
        temp <- newTemp
        code0 <- transExp tabl e1 temp
        code1 <-  transStm tabl stm1
        popTemp
        return (code0 ++ [LABEL ltrue] ++ code1 ++[LABEL lfalse])

transStm tabl (IfElseE e1 stm1 stm2)
    =do ltrue <- newLabel
        lfalse <- newLabel
        lend <- newLabel
        temp <- newTemp
        code0 <- transExp tabl e1 temp
        code1 <- transStm tabl stm1
        code2 <- transStm tabl stm2
        popTemp
        return (code0 ++ [LABEL ltrue] ++ code1 ++
                 [JUMP lend, LABEL lfalse] ++ code2 ++[LABEL lend])


transStm tabl (Block args stms ) -- list smt, args: int s; int n;
    =  do temps <- sequence [ newTemp | (PlistInt x) <- args]
          let tabl2= insertTable tabl (zip [ x | (PlistInt x) <- args] temps)
        --  arguments <-sequence [transArgs tabl2 a  | a <- args]
          list <- sequence [transStm tabl2 s | s<- stms]
          return ( concat list )



transStm tabl (Assignment assign )
  = do code <- transAssign tabl assign
       return code

-- ciclos for
transStm tabl (For assign1 comp assign2 stm)
   = do ltrue <- newLabel  -- label para stm
        lfalse <- newLabel -- sai do ciclo -> end
        code1 <- transAssign tabl assign1
        code2 <- transCond tabl comp ltrue lfalse
        code3 <- transAssign tabl assign2
        code4 <- transStm tabl stm
        return (code1 ++ code2 ++ [LABEL ltrue] ++ code3 ++
                  code4 ++ [JUMP ltrue, LABEL lfalse])


transStm tabl (Return e1 )
    = do temp <- newTemp
         code <- transExp tabl e1 temp
         popTemp
         case code of
          [MOVE dest temp]->return ([RETURNT dest])
          [MOVEI dest x] -> return ([RETURNT dest])
          [CALL dest f temps] -> return ([RETURNT dest])



transStm tabl (ReturnBool comp )
    = do ltrue <- newLabel
         lfalse <-newLabel
         code <- transCond tabl comp ltrue lfalse
         tempR <- newTemp
         popTemp
         return ([RETURNT tempR])

transStm tabl (Expre e1 )
   = do temp <- newTemp
        code <- transExp tabl e1 temp
        return code


transStm tabl (While comp stm1 )
   = do label1 <- newLabel  --label lool
        label2 <- newLabel  --label da stm1 true
        label3 <- newLabel  --lend
        code1 <- transCond tabl comp label2 label3
        code2 <- transStm tabl stm1
        return ([LABEL label1] ++code1 ++ [LABEL label2] ++ code2 ++
                   [JUMP label1, LABEL label3]  )

insertTable :: Table -> [(Ident, String)] -> Table
insertTable tabl [] = tabl
insertTable tabl ((x,temp):xs) =  insertTable ( Map.insert x temp tabl) xs



--translate declaracoes: int n; int x;
--transArgs :: Table -> Decla-> State Count [Instr]
{-transArgs tabl (PlistInt x)
   = do dest <- newTemp
        popTemp
        return ([MOVE dest x] )

transArgs tabl (PlistBool x)
    = do dest <- newTemp
         popTemp
         return ([MOVE dest x] )  -}


-- translate conditions comp
transCond :: Table -> Comp -> Label -> Label -> State Count [Instr]
transCond tabl (Cond relop e1 e2) ltrue lfalse
  = do temp1 <- newTemp
       temp2 <- newTemp
       code1 <- transExp tabl e1 temp1
       code2 <- transExp tabl e2 temp2
       popTemp
       popTemp
       return (code1 ++ code2 ++ [COND temp1 relop temp2 ltrue lfalse])


transFunc :: Funct -> State Count FunIR
transFunc (FInt id params stm) -- params = int x, int y
   = do let flabel = id -- label da funcao
        temps <- sequence [ newTemp | (PInt x) <- params]
        let tabl = Map.fromList $ zip [ x | (PInt x) <- params] temps
        stmList <-  transStm tabl stm
        return (FunIR flabel temps stmList )

--reutilizar temps
popTemp = do (temps, labels) <- get
             put (temps-1,labels)


resul ::  [Funct]-> State Count [FunIR]
resul [] = return []
resul (f:fs) = do popTemp
                  r1 <- transFunc f
                  r2 <- resul fs
                  return ( r1 :r2)
