{-# LANGUAGE BangPatterns, EmptyDataDecls, ForeignFunctionInterface #-}

-- |
-- Module:      Codec.Compression.Snappy
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module provides fast, pure zero-copy compression and
-- decompression of lazy 'ByteString' data using the Snappy format.
--
-- Although these functions operate on lazy 'ByteString's, they
-- consume the data /strictly/: they do not produce any output until
-- they have consumed all of the input, and they produce the output in
-- a single large chunk.
--
-- If your data is already in the form of a lazy 'ByteString', it is
-- likely more efficient to use these functions than to convert your
-- data to and from strict ByteStrings, as you can avoid the
-- additional allocation and copying that would entail.

module Codec.Compression.Snappy.Lazy
    (
      compress
    , decompress
    ) where

#include "hs_snappy.h"

import Codec.Compression.Snappy.Internal (maxCompressedLength)
import Data.ByteString.Internal hiding (ByteString)
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.C.Types (CInt, CSize)
import Foreign.ForeignPtr (touchForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Codec.Compression.Snappy as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

newtype BS = BS B.ByteString

instance Storable BS where
    sizeOf _    = (#size struct BS)
    alignment _ = alignment (undefined :: Ptr CInt)
    poke ptr (BS (PS fp off len)) = withForeignPtr fp $ \p -> do
      (#poke struct BS, ptr) ptr (p `plusPtr` off)
      (#poke struct BS, len) ptr len
    {-# INLINE poke #-}

-- | Compress data into the Snappy format.
compress :: ByteString -> ByteString
compress bs = unsafePerformIO $ do
  let len = fromIntegral (L.length bs)
  let dlen0 = maxCompressedLength len
  dfp <- mallocByteString dlen0
  withForeignPtr dfp $ \dptr -> do
    let chunks = L.toChunks bs
    withArray (map BS chunks) $ \chunkPtr ->
      with (fromIntegral dlen0) $ \dlenPtr -> do
        c_CompressChunks chunkPtr (fromIntegral (length chunks))
                         (fromIntegral len) dptr dlenPtr
        foldr (\(PS fp _ _) _ -> touchForeignPtr fp) (return ()) chunks
        dlen <- fromIntegral `fmap` peek dlenPtr
        if dlen == 0
          then return Empty
          else return (Chunk (PS dfp 0 dlen) Empty)

-- | Decompress data in the Snappy format.
--
-- If the input is not compressed or is corrupt, an exception will be
-- thrown.
decompress :: ByteString -> ByteString
decompress = L.fromChunks . (:[]) . S.decompress . B.concat . L.toChunks

foreign import ccall unsafe "hs_snappy.h _hsnappy_CompressChunks"
    c_CompressChunks :: Ptr BS -> CSize -> CSize -> Ptr Word8 -> Ptr CSize
                     -> IO ()
