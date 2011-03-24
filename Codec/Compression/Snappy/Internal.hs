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

module Codec.Compression.Snappy.Internal
    (
      maxCompressedLength
    ) where

import Foreign.C.Types (CSize)

maxCompressedLength :: Int -> Int
maxCompressedLength = fromIntegral . c_MaxCompressedLength . fromIntegral
{-# INLINE maxCompressedLength #-}

foreign import ccall unsafe "hs_snappy.h _hsnappy_MaxCompressedLength"
    c_MaxCompressedLength :: CSize -> CSize
