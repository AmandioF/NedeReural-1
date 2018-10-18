-- Modulo principal de Nede Reural.

module Main where
import Training
import Execution

main :: IO ()
main = do
	option <- getLine
	menu option
	return -- action apenas para satisfazer o retoro de IO ().

menu :: String -> IO String -- a decidir o tipo da IO Action
menu option
	| option == "exec" = execute
	| option == "train" = train
	| otherwise = return ("Opcao invalida!")