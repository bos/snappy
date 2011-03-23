import Codec.Compression.Snappy
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.ByteString as B

roundtrip s = decompress (compress bs) == bs
  where bs = B.pack s

main = defaultMain tests

tests = [
    testProperty "roundtrip" roundtrip
  ]
