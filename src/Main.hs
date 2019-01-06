-- | This module defines a simple command line interface for the SubScript
-- interpreter.  If your solution is correct, this module should just
-- work.
module Main
       (main)
where
import SubsAst
import SubsInterpreter
import SubsParser
  
import Data.List(intercalate)
import System.Environment(getArgs)


-- | nice display of JavaScript values
nice :: Value -> String
nice (IntVal v) = show v
nice TrueVal = "true"
nice FalseVal = "false"
nice (StringVal s) = show s
nice UndefinedVal = "undefined"
nice (ArrayVal vs) = "["++ intercalate ", " (map nice vs) ++"]"

run :: Expr -> IO ()
run e =
  case runExpr e of
    Left s -> error $ "Runtime error: " ++ s
    Right v -> putStrLn $ "Result is: " ++ nice v

main :: IO ()
main = do args <- getArgs
          case args of
            ["-i", file] -> do
              s <- readFile file
              run $ read s
            ["-p", file] -> do
              p <- parseFile file
              print p
            [file] -> do
              pp <- parseFile file
              case pp of
                Left e -> error $ show e
                Right p -> run p
            _ ->
              error "Invalid arguments!"
