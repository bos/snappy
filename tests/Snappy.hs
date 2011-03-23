{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards #-}

import Control.Exception
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as B
import System.IO
import System.Exit
import qualified Codec.Compression.Snappy as S
import System.Console.CmdArgs
import Data.Data
import Data.Typeable
import Data.Time.Clock

data Action = Compress
            | Decompress
              deriving (Eq, Show, Typeable, Data)

data Snappy = Snappy {
      action :: Action
    , number :: Maybe Int
    , files :: [FilePath]
    } deriving (Show, Typeable, Data)

snappy = Snappy { action = enum [Compress, Decompress]
                , number = Nothing
                , files = def &= args
                }

main = do
  Snappy{..} <- cmdArgs snappy
  forM_ files $ \f -> do
    bs0 <- B.readFile f
    let bs | action == Compress = bs0
           | otherwise          = S.compress bs0
    let count = fromMaybe (100000000 `div` B.length bs) number
        c !i s | i >= count = ()
               | otherwise  = S.compress s `seq` c (i+1) s
        d !i s | i >= count = ()
               | otherwise  = S.decompress s `seq` d (i+1) s
    start <- getCurrentTime
    evaluate $ if action == Compress then c 0 bs else d 0 bs
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    let time = fromRational . toRational $ delta
        mbSec = fromIntegral (B.length bs) *
                fromIntegral count / (time * 1048576.0)
    putStrLn $ show action ++ " " ++ show f ++ ": " ++ show (round mbSec) ++
               " MB/sec"
