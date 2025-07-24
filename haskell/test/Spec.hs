import Lib

main :: IO ()
main = do
  result <- runJinja
  case result of
    "{\"result\": \"Hello Marek!\", \"ok\": true}" -> putStrLn "OK: Correct greeting"
    _ -> putStrLn $ "Error: Expected 'Hello Marek!', got '" ++ result ++ "'"
