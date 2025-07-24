{-# LANGUAGE ForeignFunctionInterface #-}

module Lib (runJinja) where

import Foreign
import Foreign.C.String

-- FFI to dynamic rendering function
foreign import ccall "render_jinja"
  c_render_jinja :: CString -> IO CString

-- FFI to C's free function
foreign import ccall "free_string"
  c_free_string :: CString -> IO ()

-- Main logic wrapped as reusable function
runJinja :: IO String
runJinja = do
  let input = "{\"template\": \"Hello {{ name }}!\", \"context\": {\"name\": \"Marek\"}}"

  withCString input $ \cinput -> do
      resultCStr <- c_render_jinja cinput
      result <- peekCString resultCStr
      c_free_string resultCStr
      return result
