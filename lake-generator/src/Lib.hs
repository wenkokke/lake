{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Lib where

import Data.Aeson (encode)
import Data.ByteString (toStrict)
import Data.Maybe (fromMaybe)
import Foreign.C.ByteString (newCStringFromByteString)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal (free, fromBool)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Language.Lake.Generator (Generator)
import qualified Language.Lake.Generator as Generator

foreign export ccall lake_generator_new :: IO (StablePtr Generator)

lake_generator_new :: IO (StablePtr Generator)
lake_generator_new = newStablePtr Generator.new

foreign export ccall lake_generator_free :: StablePtr Generator -> IO ()

lake_generator_free :: StablePtr Generator -> IO ()
lake_generator_free = freeStablePtr

foreign export ccall lake_generator_has_next :: StablePtr Generator -> IO CBool

lake_generator_has_next :: StablePtr Generator -> IO CBool
lake_generator_has_next stablePtrGenerator =
  fromBool . Generator.hasNext <$> deRefStablePtr stablePtrGenerator

foreign export ccall lake_generator_value :: StablePtr Generator -> IO CString

lake_generator_value :: StablePtr Generator -> IO CString
lake_generator_value stablePtrGenerator =
  newCStringFromByteString . toStrict . encode . Generator.value
    =<< deRefStablePtr stablePtrGenerator

foreign export ccall lake_generator_value_free :: CString -> IO ()

lake_generator_value_free :: CString -> IO ()
lake_generator_value_free = free

foreign export ccall lake_generator_next :: StablePtr Generator -> IO (StablePtr Generator)

lake_generator_next :: StablePtr Generator -> IO (StablePtr Generator)
lake_generator_next stablePtrGenerator = do
  newStablePtr . fromMaybe Generator.empty . Generator.next =<< deRefStablePtr stablePtrGenerator
