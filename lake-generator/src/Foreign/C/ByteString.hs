module Foreign.C.ByteString where

import Data.ByteString.Internal (ByteString (BS))
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal (copyBytes, mallocBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))

newCStringFromByteString :: ByteString -> IO CString
newCStringFromByteString (BS foreignPtrPayload l) = do
  buffer <- mallocBytes (l + 1)
  withForeignPtr foreignPtrPayload $ \payload -> do
    copyBytes buffer payload l
    pokeByteOff buffer (l + 1) (0 :: Word8)
    return $ castPtr buffer
