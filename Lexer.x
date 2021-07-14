{
  module Lexer where

}
%wrapper "basic"
$alpha = [_a-zA-Z]
$digit = [0-9]
$white =  [\ \t\n\r]

tokens :-
$white+                       ; --ignorar espaços em branco
[\/]+.*                       ; --ignorar comentarios
if                            {\_ -> IF}
else 						  {\_ -> ELSE}
int                           {\_ -> INT}
while                         {\_ -> WHILE}
true				          {\_ -> TRUE}
false                         {\_ -> FALSE}
bool                          {\s -> BOOL }
return                        {\_ -> RETURN}
"!="                          {\_ -> BNE}
$alpha($alpha|$digit)*        {\s -> ID s}
$digit+                       {\s -> NUM (read s)}
[\,]                          {\_ -> COMMA}
[\(]                          {\_ -> LPAREN}
[\)]                          {\_ -> RPAREN}
[\{]                          {\_ -> LBRACE}
[\}]                          {\_ -> RBRACE}
[\;]                          {\_ -> SEMICOLON}
[\+]                          {\_ -> PLUS}
[\-]                          {\_ -> MINUS}
[\*]                          {\_ -> MULT}
[\/]                          {\_ -> DIV}
[\%]                          {\_ -> MODULAR}
"="                           {\_ -> ASSIGN}
==                            {\_ -> BEQ}
[\<]                          {\_ -> MINOR}
"<="                          {\_ -> MINOREQUAL}
[\>]                          {\_ -> MAJOR}
">= "                         {\_ -> MAJOREQUAL}
for                           {\_ -> FOR}



{
data Token = IF
             | ELSE
             | ID String
             | NUM Int
             | TRUE
             | FALSE
             | LPAREN
             | RPAREN
             | COMMA
             | LBRACE
             | RBRACE
             | SEMICOLON
             | WHILE
             | RETURN
             | FOR
             | INT
             | PLUS
             | MINUS
             | MULT
             | DIV
             | MODULAR
             | ASSIGN
             | BNE
             | MINOREQUAL
             | MINOR
             | MAJOR
             | MAJOREQUAL
             | BOOL
             | BEQ
             deriving (Eq, Show)
}
