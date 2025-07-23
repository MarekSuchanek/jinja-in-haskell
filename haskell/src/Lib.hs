module Lib
    ( renderGreeting
    ) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (Value, (.=), object, encode)
import Data.Time.Clock (getCurrentTime)
import System.Exit (ExitCode(..))
import System.IO
import System.Process


renderJinjaBin :: FilePath
renderJinjaBin = "render-jinja"


renderGreeting :: String -> IO (Either String String)
renderGreeting name = do
  let template = "Hello, {{ name }}!"
      context = object ["name" .= name]
  result <- renderWithJinja template context
  return result

-- | Renders a Jinja template using an external binary.
-- Takes: JSON context
-- Returns: Either error or rendered string
renderWithJinja :: String -> Value -> IO (Either String String)
renderWithJinja templateStr contextJson = do
  let input = object ["template" .= templateStr, "context" .= contextJson]
  let inputJson = encode input

  result <- try $ do
    startTime <- getCurrentTime
    -- Start the subprocess with template string as argument
    let processSpec = (proc renderJinjaBin [])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }

    (Just hIn, Just hOut, Just hErr, pHandle) <- createProcess processSpec

    -- Send JSON input to stdin
    BL.hPutStr hIn inputJson
    hClose hIn

    -- Read output and error
    output <- hGetContents hOut
    errput <- hGetContents hErr
    exitCode <- waitForProcess pHandle
    endTime <- getCurrentTime

    print startTime
    print endTime

    case exitCode of
      ExitSuccess   -> return $ Right output
      ExitFailure _ -> return $ Left $ "render-jinja failed: " ++ errput

  -- Catch any exceptions and wrap in Left
  case result of
    Left ex  -> return $ Left $ "Exception: " ++ show (ex :: SomeException)
    Right ok -> return ok
