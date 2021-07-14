module Final where
import Parser
import Lexer
import Interm
import Mcp

mips :: [String] -> String
mips [] = ""
mips (x:xs) = x ++ " " ++ mips xs