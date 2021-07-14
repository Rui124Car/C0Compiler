module Main where
import Parser
import Lexer
import Interm
import Mcp
import Final
import Control.Monad.State

main :: IO ()
main = do
  txt <- getContents
  let list = happyParser $ alexScanTokens txt
  let interm = evalState (resul list) (1,0)
  let cm = start interm
  let final = mips cm
  putStrLn (final)
