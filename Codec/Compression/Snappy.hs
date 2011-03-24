{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module:      Codec.Compression.Snappy
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module provides fast, pure Haskell bindings to Google's
-- Snappy compression and decompression library:
-- <http://code.google.com/p/snappy/>
--
-- These functions operate on strict bytestrings, and thus use as much
-- memory as both the entire compressed and uncompressed data.

module Codec.Compression.Snappy
    (
      compress
    , decompress
    ) where

import Codec.Compression.Snappy.Internal (maxCompressedLength)
import Control.Monad (when)
import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Data.Word (Word8)
import Foreign.C.Types (CInt, CSize)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B

-- | Compress data into the Snappy format.
compress :: ByteString -> ByteString
compress bs@(PS sfp off len) = unsafePerformIO $ do
  let dlen0 = maxCompressedLength len
  dfp <- mallocByteString dlen0
  withForeignPtr sfp $ \sptr ->
    withForeignPtr dfp $ \dptr ->
      with (fromIntegral dlen0) $ \dlenPtr -> do
        c_RawCompress (sptr `plusPtr` off) (fromIntegral len) dptr dlenPtr
        (PS dfp 0 . fromIntegral) `fmap` peek dlenPtr

-- | Decompress data in the Snappy format.
--
-- If the input is not compressed or is corrupt, an exception will be
-- thrown.
decompress :: ByteString -> ByteString
decompress (PS sfp off slen) = unsafePerformIO $
  withForeignPtr sfp $ \sptr0 -> do
    let sptr = sptr0 `plusPtr` off
        len = fromIntegral slen
    let check ok = when (ok == 0) $
                   fail "Codec.Compression.Snappy.decompress: corrupt input"
    alloca $ \dlenPtr -> do
      check =<< c_GetUncompressedLength sptr len dlenPtr
      dlen <- fromIntegral `fmap` peek dlenPtr
      dfp <- mallocByteString dlen
      withForeignPtr dfp $ \dptr -> do
        check =<< c_RawUncompress sptr len dptr
        return (PS dfp 0 dlen)

foreign import ccall unsafe "hs_snappy.h _hsnappy_RawCompress"
    c_RawCompress :: Ptr a -> CSize -> Ptr Word8 -> Ptr CSize -> IO ()

foreign import ccall unsafe "hs_snappy.h _hsnappy_GetUncompressedLength"
    c_GetUncompressedLength :: Ptr a -> CSize -> Ptr CSize -> IO CInt

foreign import ccall unsafe "hs_snappy.h _hsnappy_RawUncompress"
    c_RawUncompress :: Ptr a -> CSize -> Ptr Word8 -> IO CInt
