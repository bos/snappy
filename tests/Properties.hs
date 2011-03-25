{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative
import qualified Codec.Compression.Snappy as B
import qualified Codec.Compression.Snappy.Lazy as L
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary

instance Arbitrary L.ByteString where
    arbitrary = rechunk <$> arbitrary <*> arbitrary

s_roundtrip bs = B.decompress (B.compress bs) == bs

newtype Compressed a = Compressed { compressed :: a }
    deriving (Eq, Ord)

instance Show a => Show (Compressed a)
    where show (Compressed a) = "Compressed " ++ show a

instance Arbitrary (Compressed B.ByteString) where
    arbitrary = (Compressed . B.compress) <$> arbitrary

compress_eq n bs = L.fromChunks [B.compress bs] == L.compress (rechunk n bs)
decompress_eq n bs0 =
    L.fromChunks [B.decompress bs] == L.decompress (rechunk n bs)
  where bs = B.compress bs0

rechunk :: Int -> B.ByteString -> L.ByteString
rechunk n = L.fromChunks . go
  where go bs | B.null bs = []
              | otherwise = case B.splitAt ((n `mod` 63) + 1) bs of
                              (x,y) -> x : go y

t_rechunk n bs = L.fromChunks [bs] == rechunk n bs

l_roundtrip bs = L.decompress (L.compress bs) == bs

main = defaultMain tests

tests = [
    testProperty "s_roundtrip" s_roundtrip
  , testProperty "t_rechunk" t_rechunk
  , testProperty "compress_eq" compress_eq
  , testProperty "decompress_eq" decompress_eq
  , testProperty "l_roundtrip" l_roundtrip
  ]
