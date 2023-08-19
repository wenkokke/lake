module Lib where

import Data.Maybe (listToMaybe)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CUShort (..), CULong (..))
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import Language.Lake.AST (AST)
import Language.Lake.AST.DeBruijn (Z)
import Language.Lake.Generator (allValues)

data Generator = Generator
  { size :: Int,
    count :: Integer,
    values :: [AST Z],
    next :: [(Integer, [AST Z])]
  }

foreign export ccall newGenerator :: IO (StablePtr Generator)

emptyGenerator :: Generator
emptyGenerator = Generator
  {
    size = 0,
    count = 0,
    values = [],
    next = []
  }

newGenerator :: IO (StablePtr Generator)
newGenerator = newStablePtr emptyGenerator { next = allValues }

foreign export ccall freeGenerator :: StablePtr Generator -> IO ()

freeGenerator :: StablePtr Generator -> IO ()
freeGenerator = freeStablePtr 

foreign export ccall getSize :: StablePtr Generator -> IO CUShort

getSize :: StablePtr Generator -> IO CUShort
getSize stablePtrGenerator = do
  generator <- deRefStablePtr stablePtrGenerator
  return . fromIntegral $ size generator

foreign export ccall getCount :: StablePtr Generator -> IO CULong

getCount :: StablePtr Generator -> IO CULong
getCount stablePtrGenerator = do
  generator <- deRefStablePtr stablePtrGenerator
  return . fromIntegral $ count generator

foreign export ccall getValue :: StablePtr Generator -> IO CString

getValue :: StablePtr Generator -> IO CString
getValue stablePtrGenerator = do
  generator <- deRefStablePtr stablePtrGenerator
  let valueString = maybe "" show (listToMaybe $ values generator)
  newCString valueString

foreign export ccall nextValue :: StablePtr Generator -> IO (StablePtr Generator)

nextValue :: StablePtr Generator -> IO (StablePtr Generator)
nextValue stablePtrGenerator = do
  generator <- deRefStablePtr stablePtrGenerator
  let nextCount = max 0 (count generator + 1)
  let nextValues = drop 1 (values generator)
  newStablePtr generator
    {
      count = nextCount,
      values = nextValues
    }

foreign export ccall nextSize :: StablePtr Generator -> IO (StablePtr Generator)

nextSize :: StablePtr Generator -> IO (StablePtr Generator)
nextSize stablePtrGenerator = do
  generator <- deRefStablePtr stablePtrGenerator
  let nextGenerator =
        case next generator of
          [] -> emptyGenerator
          ((nextCount, nextValues) : nextNext) ->
            Generator
              {
                size = succ (size generator),
                count = nextCount,
                values = nextValues,
                next = nextNext
              }
  newStablePtr nextGenerator