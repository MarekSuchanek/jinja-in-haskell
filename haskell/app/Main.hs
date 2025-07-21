module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  result <- renderGreeting name
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right str -> putStrLn str
