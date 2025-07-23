module Lib
    ( renderGreeting
    ) where

import Foreign.C.String

foreign import ccall "render_jinja"
  c_render_jinja :: CString -> CString -> IO CString

renderJinja :: String -> String -> IO String
renderJinja tmpl context =
  withCString tmpl $ \tmplC ->
    withCString context $ \ctxC -> do
      resultC <- c_render_jinja tmplC ctxC
      peekCString resultC

renderGreeting :: String -> IO String
renderGreeting name = do
  let tmpl = "Hello {{ name }}, {{ greeting }}!"
  let context = "{ \"name\": \"" ++ name ++ "\" }"
  renderJinja tmpl context
