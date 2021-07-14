{
module Parser where
import Lexer
}

%name happyParser -- nome da funcao de parsing a qual chamar para executar
%tokentype { Token }  -- tipo de tokens do lexer.hs
%error {parseError} -- funcao p reportar erros

%token   -- sintaxe na gramatica para token

num             {NUM $$}
id              {ID $$}
'+'             {PLUS}
'*'             {MULT}
'/'             {DIV}
'-'             {MINUS}
'%'             {MODULAR}
'('             {LPAREN}
')'             {RPAREN}
'='             {ASSIGN}
';'             {SEMICOLON}
','             {COMMA}
if              {IF}
else            {ELSE}
int             {INT}
while           {WHILE}
true            {TRUE}
false           {FALSE}
return          {RETURN}
bool            {BOOL}
"!="            {BNE}
"=="            {BEQ}
'<'             {MINOR}
"<="            {MINOREQUAL}
'>'             {MAJOR}
">="            {MAJOREQUAL}
'{'             {LBRACE}
'}'             {RBRACE}
for             {FOR}


-- precedencias dos operadores

%nonassoc '>' '<' '>=' '<=' '=' '!=' '=='
%left '+' '-'
%left '*' '/' '%'



%% --gramatica


-- varios blocos de funcoes
FunctList : Funct FunctList              {$1 : $2}
          |                              {[]}
-- funcoes e main
Funct : int id '(' ParamF  ')'  Stm       { FInt $2 $4 $6}    -- Stm ja inclui {}
      | bool id '(' ParamF  ')' Stm       { FBool $2 $4 $6}


--declaracoes e iniciacao de variaveis
Decla : int id               { PlistInt  $2}
      | bool id               { PlistBool $2}


--varias variaveis
--ParamL : id ParamR                { $1 : $2} -- int a,n ,i;

-- ParamR : ',' id ParamR            { $2 : $3}
--       |                          {[]}

-- parametros para iniciar funcoes
Parametro : int id                   { PInt $2}  --int a
          | bool id                  { PBool $2}

ParamF : Parametro ParamFList         { $1 : $2}
       |                              {[]}


ParamFList : ',' Parametro ParamFList   { $2 : $3}
           |                            {[]}

Assign : id '=' Exp     {ExpA $1 $3}

-- expressoes aritmeticas
Exp : Exp '+' Exp        {Op Add $1 $3}
    | Exp '-' Exp        {Op Minus $1  $3}
    | Exp '*' Exp        {Op Mult $1 $3}
    | Exp '/' Exp        {Op Div $1 $3}
    | Exp '%' Exp        {Op Mod $1 $3}
    | '(' Exp ')'        {BtweenParent $2}
    | num                {Num $1}
    | id                 {Id $1}
    | true               {Bool True}
    | false              {Bool False}
    | id '(' ExpL ')'    {CallFunct $1 $3}

ExpL : Exp ExpR      {$1 : $2}
     |               {[]}

ExpR : ',' Exp ExpR      {$2: $3}
     |               {[]}


Comp : Exp "!=" Exp       {Cond Bne $1 $3 }
     | Exp "==" Exp       {Cond Beq $1 $3}
     | Exp '>' Exp        {Cond Bgt $1 $3}
     | Exp '<' Exp        {Cond Blt $1 $3}
     | Exp "<=" Exp       {Cond Ble $1 $3}


Stm : if '(' Comp')' Stm                        { If $3  $5 }
    | if '(' Comp')' Stm else Stm               { IfElse $3  $5 $7}
    | if '(' Exp ')' Stm                      { IfE $3  $5 }
    | if '(' Exp ')' Stm  else Stm             { IfElseE $3  $5 $7}
    | while '(' Comp ')' Stm                    { While $3  $5}
    | for '(' Assign ';' Comp ';' Assign ')' Stm  { For $3 $5 $7 $9 }
    | '{' DeclaL StmL '}'                            {Block $2 $3 }
    | Assign ';'                                {Assignment $1}
    | Exp ';'                                      {Expre $1}
    | return Exp ';'                            { Return $2}
    | return Comp ';'                           { ReturnBool $2 }
  
DeclaL : Decla ';' DeclaL        {$1: $3}
       |                        {[]}



StmL : Stm StmL               { $1 : $2}
     |                        { [] } --  epsilon

{
parseError :: [Token] -> a
parseError toks =error ( "parse error at " ++ show toks )


-- para operacoes aritmeticas
data Exp = Op BinOp  Exp  Exp
         | Num Int
         | BtweenParent Exp
         | Bool Bool
         | Id String
         | CallFunct String [Exp]
        deriving Show

data Comp = Cond RelOp Exp  Exp
          | LOr Exp Exp
          | LAnd Exp Exp
          deriving Show

data Assign = ExpA String Exp 
            deriving Show

data Stm = If Comp Stm
         | IfElse Comp Stm  Stm
         | IfE Exp Stm
         | IfElseE Exp Stm  Stm
         | Block [Decla] [Stm] --
         | StmExp Exp
         | While Comp Stm
         | For Assign Comp Assign Stm 
         | Stm Stm
         | Return Exp
         | ReturnBool Comp
         | Assignment Assign
         | Expre Exp
        deriving Show

data Funct = FInt  String [Parametro] Stm
           | FBool String [Parametro] Stm
          deriving Show

data Decla = PlistInt String
           | PlistBool String
           deriving Show



data Parametro = PInt String
               | PBool String
             deriving Show

data BinOp = Add| Minus| Mult | Div | Mod
   deriving Show

data RelOp= Bne | Beq | Bgt | Blt | Ble | Bge
        deriving Show
}
