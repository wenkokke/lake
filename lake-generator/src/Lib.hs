{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Lib where

import Data.Maybe (fromMaybe)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal (fromBool)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Language.Lake.Generator
  ( Generator (..),
    emptyGenerator,
    getNext,
    getValue,
    hasNext,
    newGenerator,
  )

foreign export ccall lake_generator_new :: IO (StablePtr Generator)

lake_generator_new :: IO (StablePtr Generator)
lake_generator_new = newStablePtr newGenerator

foreign export ccall lake_generator_free :: StablePtr Generator -> IO ()

lake_generator_free :: StablePtr Generator -> IO ()
lake_generator_free = freeStablePtr

foreign export ccall lake_generator_has_next :: StablePtr Generator -> IO CBool

lake_generator_has_next :: StablePtr Generator -> IO CBool
lake_generator_has_next stablePtrGenerator =
  fromBool . hasNext <$> deRefStablePtr stablePtrGenerator

foreign export ccall lake_generator_value :: StablePtr Generator -> IO CString

lake_generator_value :: StablePtr Generator -> IO CString
lake_generator_value stablePtrGenerator = do
  newCString . maybe "" show . getValue =<< deRefStablePtr stablePtrGenerator

foreign export ccall lake_generator_next :: StablePtr Generator -> IO (StablePtr Generator)

lake_generator_next :: StablePtr Generator -> IO (StablePtr Generator)
lake_generator_next stablePtrGenerator = do
  newStablePtr . fromMaybe emptyGenerator . getNext =<< deRefStablePtr stablePtrGenerator
