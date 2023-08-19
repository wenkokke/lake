module Lib where

import Foreign.C.String (CString, newCString)
import Language.Lake.Generator (allValues)

foreign export ccall generate :: IO CString

generate :: IO CString
generate = do
  let value = head (snd (head allValues))
  newCString (show value)
