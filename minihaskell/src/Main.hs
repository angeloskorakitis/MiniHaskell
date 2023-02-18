{- Usage: ./a.out < input.txt -}

module Main where

import Intensional (eval)
import Parser (programParser)
-- import Parser_Bonus (programParser)
import Text.Pretty.Simple (pPrint)
import Transform (transform)
import Types ( IExpr(INum), IExpr(IBool) )

main :: IO ()
main = do
    contents <- getContents
    case programParser contents of
        Left err -> error "> Parse Error"
        Right fp ->
            do
                putStrLn "> Pretty-Print the MiniHaskell AST:\n"
                pPrint fp
                putStrLn "\n\n> Pretty-Print the Intensional AST:\n"
                pPrint (transform fp)
                putStrLn "\n\n> Result is:"
                case eval (transform fp) of
                    INum n -> print n
                    IBool n -> print n
                    _ -> error "> Runtime Error"
