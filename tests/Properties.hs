import qualified Codec.Compression.Snappy as B
import qualified Codec.Compression.Snappy.Lazy as L
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

s_roundtrip s = B.decompress (B.compress bs) == bs
  where bs = B.pack s

l_roundtrip s = L.decompress (L.compress bs) == bs
  where bs = L.pack s

main = defaultMain tests

tests = [
    testProperty "s_roundtrip" s_roundtrip
  , testProperty "l_roundtrip" l_roundtrip
  ]
