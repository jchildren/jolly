module Main where

import           System.Console.Haskeline
import           Text.Megaparsec

import           Parser
import           Pretty
import           Syntax
import           Eval

process :: String -> String
process line = do
  let res = parse parseExpr "" line
  case res of
    Left err -> parseErrorPretty err
    Right ex -> ppval $ eval emptyEnv ex

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "Jolly> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          outputStrLn $ process input
          loop
